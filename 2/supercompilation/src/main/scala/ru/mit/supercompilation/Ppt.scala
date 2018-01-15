package ru.mit.supercompilation

import ru.mit.supercompilation.Types._
import ru.mit.supercompilation.reducer.Reducer
import ru.mit.supercompilation.reducer.Types.NormalizedExpr

import scala.collection.mutable

/**
  * Partial process tree
  */
class Ppt(val prog: Program) {

  val root = PptNode(null, normalize(prog._1))
  val unprocessedLeafs = new mutable.HashSet[PptNode]()
  unprocessedLeafs.add(root)

  type FunDescriptor = (String, Seq[Int])
  val generatedFunction = new mutable.HashMap[PptNode, FunDescriptor]()

  private[this] def drive(node: PptNode): Unit = {
    if (node.children.nonEmpty || node.link != null) {
      throw new IllegalArgumentException("node should be an unprocessed leaf")
    }

    val children: List[NormalizedExpr] = node.expr match {
      case Left(v) if v.isInstanceOf[Var] =>
        Nil
      case Left(a@App(_, _)) =>
        val fApp = flattenApp(a)
        if (!fApp.head.isInstanceOf[Var]) {
          throw new IllegalArgumentException("Node expression is not normalized")
        }
        fApp.tail.map(e => normalize(e))
      case Left(Constr(_, exprs)) =>
        exprs.map(e => normalize(e))
      case Left(Lambda(e)) =>
        List(normalize(e))
      case e@Right((Fun(_), _)) =>
        List(Reducer.nReduceStep(e, prog._2))
      case e@Right((App(Lambda(_), _), _)) =>
        List(Reducer.nReduceStep(e, prog._2))
      case e@Right((Case(Constr(_, _), _), _)) =>
        List(Reducer.nReduceStep(e, prog._2))
      case Right((Case(selector, cases), ctx)) =>
        List(normalize(selector)) ++ cases.map(c => normalize(c._3, ctx))
      case Right((Let(subst, e2), _)) =>
         List(normalize(e2)) ++ subst.map(e => normalize(e._2))
      case _ => throw new IllegalArgumentException("Node expression is not normalized")
    }

    node.addChildren(children)
  }

  private[this] def tryInstance(node: PptNode): Boolean = {
    var anc = node.parent
    while (anc != null) {
      isInstance(toExpr(node.expr), toExpr(anc.expr)) match {
        case Some(subst) =>
          node.expr = normalize(Let(subst, toExpr(anc.expr)))
          node.addChildren(anc.expr +: subst.map(sub => normalize(sub._2)))
          node.children.head.fold(PptLink(anc, subst))
          return true
        case _ => anc = anc.parent
      }
    }
    false
  }

  private[this] def removeSubtree(node: PptNode): Unit = {
    if (node.isLeaf) {
      node.unfold()
      unprocessedLeafs.remove(node)
    } else {
      node.children.foreach(child => removeSubtree(child))
      node.children = null
    }
  }

  private[this] def tryGeneralization(node: PptNode): Boolean = {
    var anc = node.parent
    while (anc != null) {
      if (isEmbedded(toExpr(anc.expr), toExpr(node.expr))) {
        val generalization = generalize(toExpr(anc.expr), toExpr(node.expr))
        val newExpr = Let(generalization._2, generalization._1)
        val newNode = PptNode(anc.parent, normalize(newExpr))
        removeSubtree(anc)
        anc.parent.resetChild(anc, newNode)
        unprocessedLeafs.add(newNode)
        return true
      } else {
        anc = anc.parent
      }
    }
    false
  }

  def build(): Unit = {
    while (unprocessedLeafs.nonEmpty) {
      val leaf = unprocessedLeafs.iterator.next()
      unprocessedLeafs.remove(leaf)
      if (!tryInstance(leaf)) {
        if (!tryGeneralization(leaf)) {
          drive(leaf)
        }
      }
    }
  }

  private[this] def residualizeLinked(node: PptNode): Program = {
    if (node.isLeaf) {
      val fun = generatedFunction.apply(node.link.to)._1
      val args = generatedFunction.apply(node.link.to)._2

      val mainExpr = args.foldLeft[Expr](Fun(fun)) { (prevExpr, id) =>
        node.link.subst.find(s => s._1.id == id) match {
          case None => App(prevExpr, ConfVar(id))
          case Some(s) => App(prevExpr, s._2)
        }
      }
      (mainExpr, Nil)
    } else {
      val fun = "f" + nextFunId
      val args = node.linkedByConf.toSeq.filter(elem => elem._2 != 0).map(_._1)
      generatedFunction.+((node, (fun, args)))

      val unlinkedRes = residualizeUnlinked(node)
      val body = unlinkedRes._1
      var wrappedBody: Expr = body
      var confSubst: List[Expr] = Nil
      (0 to args.size).foreach { id =>
        wrappedBody = Lambda(wrappedBody)
        confSubst = BVar(id) :: confSubst
      }
      confSubst = confSubst.reverse

      val s: Substitution = args.map(id => ConfVar(id)).zip(confSubst).toList

      val fdef = (fun, subst(wrappedBody, s))
      val mainExpr = args.foldLeft[Expr](Fun(fun)) { (prevExpr, id) =>
        App(prevExpr, ConfVar(id))
      }

      (mainExpr, fdef :: unlinkedRes._2)
    }
  }

  private[this] def residualizeUnlinked(node: PptNode): Program = {
    val childrenRes = node.children.map(child => residualize(child))
    val childrenExprs = childrenRes.map(_._1)
    var childrenFDefs = childrenRes.flatMap(_._2)

    val resExpr: Expr = node.expr match {
      case Left(v) if v.isInstanceOf[Var] =>
        v
      case Left(a@App(_, _)) =>
        val fApp = flattenApp(a)
        if (!fApp.head.isInstanceOf[Var]) {
          throw new IllegalArgumentException("Node expression is not normalized")
        }
        childrenExprs.foldLeft(fApp.head) { (e1, e2) => App(e1, e2) }
      case Left(Constr(name, _)) =>
        Constr(name, childrenExprs)
      case Left(Lambda(_)) =>
        Lambda(childrenExprs.head)
      case Right((Fun(_), _)) =>
        childrenExprs.head
      case Right((App(Lambda(_), _), _)) =>
        childrenExprs.head
      case Right((Case(Constr(_, _), _), _)) =>
        childrenExprs.head
      case Right((Case(_, cases), _)) =>
        val newCases = cases.zip(childrenExprs).map(c => (c._1._1, c._1._2, c._2))
        Case(childrenExprs.head, newCases)
      case Right((Let(s, e), _)) =>
        val newSubst = s.map(_._1).zip(childrenExprs.tail)
        childrenFDefs = childrenFDefs.map(fdef => (fdef._1, subst(fdef._2, newSubst)))
        subst(e, s)
      case _ => throw new IllegalArgumentException("Node expression is not normalized")
    }

    (resExpr, childrenFDefs)
  }

  private[this] def residualize(node: PptNode): Program = {
    if (node.isFolded) {
      residualizeLinked(node)
    } else {
      residualizeUnlinked(node)
    }
  }

  def residualize(): Program = {
    residualize(root)
  }

  case class PptNode(parent: PptNode, var expr: NormalizedExpr) {

    // The order for children nodes is important
    var children: List[PptNode] = Nil
    var link: PptLink = _
    val linkedBy = new mutable.HashSet[PptNode]()
    val linkedByConf: mutable.SortedMap[Int, Int] = mutable.SortedMap.empty[Int, Int]

    def addChildren(childrenExpr: List[NormalizedExpr]): Unit = {
      children = childrenExpr.map(childExpr => PptNode(this, childExpr))
      children.foreach(child => unprocessedLeafs.add(child))
    }

    def resetChild(oldChild: PptNode, newChild: PptNode): Unit = {
      var newChildren: List[PptNode] = Nil
      for (child <- children) {
        if (child == oldChild) {
          newChildren = newChild :: newChildren
        } else {
          newChildren = child :: newChildren
        }
      }
      children = newChildren.reverse
    }

    def isLeaf: Boolean = {
      children.nonEmpty
    }

    def fold(link: PptLink): Unit = {
      this.link = link
      link.to.linkedBy.add(this)
      link.subst.foreach(s => linkedByConf.+((s._1.id, linkedByConf.getOrElse(s._1.id, 0) + 1)))
    }

    def unfold(): Unit = {
      if (link != null) {
        link.to.linkedBy.remove(this)
        link.subst.foreach(s => linkedByConf.+((s._1.id, linkedByConf.apply(s._1.id) - 1)))
        link = null
      }
    }

    def isFolded: Boolean = {
      link != null || linkedBy.nonEmpty
    }

    private def depth(): Int = {
      if (parent == null) {
        4
      } else {
        4 + parent.depth()
      }
    }

    override def toString: String = {
      var s: String = ""
      s = expr.toString
      for (child <- children) {
        s += "\n"
        0.until(depth()).foreach {_ =>
          s += " "
        }
        s += child.toString
      }
      s
    }

  }

  case class PptLink(to: PptNode, subst: Substitution)

}
