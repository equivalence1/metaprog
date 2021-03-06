package ru.mit.supercompilation

import ru.mit.supercompilation.Types._

import scala.collection.mutable

/**
  * Partial process tree
  */
class Ppt(val prog: Program) {

  var root = PptNode(null, normalize(prog.mainExpr))
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
        List(Reducer.nReduceStep(e, prog.fdefs))
      case e@Right((App(Lambda(_), _), _)) =>
        List(Reducer.nReduceStep(e, prog.fdefs))
      case e@Right((Case(Constr(_, _), _), _)) =>
        List(Reducer.nReduceStep(e, prog.fdefs))
      case Right((Case(selector, cases), ctx)) =>
        List(normalize(selector)) ++ cases.map(c => normalize(c.expr, ctx))
      case Right((Let(subst, e2), _)) =>
         List(normalize(e2)) ++ subst.map(e => normalize(e._2))
      case _ => throw new IllegalArgumentException("Node expression is not normalized")
    }

    node.addChildren(children)
  }

  private[this] def tryFold(anc: PptNode, leaf: PptNode): Boolean = {
    val leafExpr = toExpr(leaf.expr)
    val ancExpr = toExpr(anc.expr)
    val leafToAnc = isInstance(leafExpr, ancExpr)
    val ancToLeaf = isInstance(ancExpr, leafExpr)
    (leafToAnc, ancToLeaf) match {
      case (Some(subst), Some(_)) =>
        leaf.fold(PptLink(anc, subst))
        true
      case _ => false
    }
  }

  private[this] def replace(oldNode: PptNode, newNode: PptNode): Unit = {
    def removeSubtree(node: PptNode): Unit = {
      if (node.isLeaf) {
        node.unfold()
        unprocessedLeafs.remove(node)
      } else {
        node.children.foreach(child => removeSubtree(child))
        node.children = null
      }
    }

    removeSubtree(oldNode)
    if (oldNode != root) {
      oldNode.parent.replaceChild(oldNode, newNode)
    } else {
      root = newNode
    }
  }

  private[this] def doAbstract(node1: PptNode, node2: PptNode): Unit = {
    val generalization = generalize(toExpr(node1.expr), toExpr(node2.expr))
    val newExpr = Let(generalization.subst1, generalization.gExpr)
    val newNode = PptNode(node1.parent, normalize(newExpr))
    replace(node1, newNode)
    unprocessedLeafs.add(newNode)
  }

  private[this] def tryAbstract(anc: PptNode, leaf: PptNode): Boolean = {
    isInstance(toExpr(leaf.expr), toExpr(anc.expr)) match {
      case Some(_) =>
        doAbstract(leaf, anc)
        true
      case _ => false
    }
  }

  private[this] def computeRelAncs(node: PptNode): List[PptNode] = {
    node.ancestors.filter(n => isCycleCandidate(n.expr)).toList
  }

  private[this] def findCandidateAnc(nodes: List[PptNode], leaf: PptNode): Option[PptNode] = {
    nodes.find(node => isEmbedded(toExpr(node.expr), toExpr(leaf.expr)) &&
      getExprClass(node.expr) == getExprClass(leaf.expr))
  }

  /*
    Actually, split stage is a bit more complicated then just
    drive. But for now just driving is enough.
   */
  private[this] def split(node: PptNode, expr: NormalizedExpr): Unit = {
    drive(node)
  }

  /**
    * Builds partial process tree.
    */
  def build(): Unit = {
    while (unprocessedLeafs.nonEmpty) {
      val leaf = unprocessedLeafs.iterator.next()
      unprocessedLeafs.remove(leaf)

      val relAncs = computeRelAncs(leaf)
      val findRes = findCandidateAnc(relAncs, leaf)

      // TODO this is messy
      findRes match {
        case Some(anc) =>
          if (!tryFold(anc, leaf)) {
            if (!tryAbstract(anc, leaf)) {
              generalize(toExpr(anc.expr), toExpr(leaf.expr)) match {
                case Generalization(e, _, _) if !e.isInstanceOf[ConfVar] =>
                  doAbstract(anc, leaf)
                case _ => split(leaf, leaf.expr)
              }
            }
          }
        case _ =>
          drive(leaf)
      }
    }
  }

  private[this] def residualizeLinked(node: PptNode): Program = {
    if (node.isLeaf) {
      val fun = generatedFunction.apply(node.link.to)._1
      val args = generatedFunction.apply(node.link.to)._2

      val mainExpr = args.foldLeft[Expr](Fun(fun)) { (prevExpr, id) =>
        node.link.subst.find(s => s._1.id == id) match {
          case Some(s) => App(prevExpr, s._2)
          case None => App(prevExpr, ConfVar(id))
        }
      }

      Program(mainExpr, Nil)
    } else {
      val fun = "f" + nextFreeFunIndex()
      val args = node.linkedByConf.toSeq.filter(elem => elem._2 != 0).map(_._1)
      generatedFunction.+=((node, (fun, args)))

      var confSubst: List[Expr] = Nil
      args.indices.foreach { id =>
        confSubst = BVar(id) :: confSubst
      }

      val unlinkedRes = residualizeUnlinked(node)
      val body = unlinkedRes.mainExpr
      val s: Substitution = args.map(id => ConfVar(id)).zip(confSubst).toList
      var wrappedBody: Expr = subst(body, s)
      args.indices.foreach { _ =>
        wrappedBody = Lambda(wrappedBody)
      }

      val fdef = FDef(fun, wrappedBody)
      val mainExpr = args.foldLeft[Expr](Fun(fun)) { (prevExpr, id) =>
        if (id >= 0) {
          App(prevExpr, ConfVar(id))
        } else {
          App(prevExpr, BVar(-id - 1))
        }
      }

      Program(mainExpr, fdef :: unlinkedRes.fdefs)
    }
  }

  private[this] def residualizeUnlinked(node: PptNode): Program = {
    val childrenRes = node.children.map(child => residualize(child))
    val childrenExprs = childrenRes.map(_.mainExpr)
    var childrenFDefs = childrenRes.flatMap(_.fdefs)

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
        val newCases = cases.zip(childrenExprs.tail).map(c => CaseBranch(c._1.constrName, c._1.nrArgs, c._2))
        Case(childrenExprs.head, newCases)
      case Right((Let(s, _), _)) =>
        val newSubst = s.map(_._1).zip(childrenExprs.tail)
        childrenFDefs = childrenFDefs.map(fdef => FDef(fdef.fName, subst(fdef.body, newSubst)))
        subst(childrenExprs.head, newSubst)
      case _ => throw new IllegalArgumentException("Node expression is not normalized")
    }

    Program(resExpr, childrenFDefs)
  }

  private[this] def residualize(node: PptNode): Program = {
    if (node.isFolded) {
      residualizeLinked(node)
    } else {
      residualizeUnlinked(node)
    }
  }

  /**
    * After tree has been built, this method translates it to final [[Program]].
    */
  def residualize(): Program = {
    residualize(root)
  }

  case class PptNode(parent: PptNode, expr: NormalizedExpr) {

    // The order for children nodes is important
    var children: List[PptNode] = Nil
    var link: PptLink = _
    val linkedBy = new mutable.HashSet[PptNode]()
    val linkedByConf: mutable.SortedMap[Int, Int] = mutable.SortedMap.empty[Int, Int]

    def addChildren(childrenExpr: List[NormalizedExpr]): Unit = {
      children = childrenExpr.map(childExpr => PptNode(this, childExpr))
      children.foreach(child => unprocessedLeafs.add(child))
    }

    def replaceChild(oldChild: PptNode, newChild: PptNode): Unit = {
      children = for (child <- children) yield {
        if (child == oldChild) {
          newChild
        } else {
          child
        }
      }
    }

    def isLeaf: Boolean = {
      children.isEmpty
    }

    def fold(link: PptLink): Unit = {
      this.link = link
      link.to.linkedBy.add(this)
      link.subst.foreach(s => link.to.linkedByConf.+=((s._1.id, link.to.linkedByConf.getOrElse(s._1.id, 0) + 1)))
    }

    def unfold(): Unit = {
      if (link != null) {
        link.to.linkedBy.remove(this)
        link.subst.foreach(s => link.to.linkedByConf.+=((s._1.id, link.to.linkedByConf.apply(s._1.id) - 1)))
        link = null
      }
    }

    def isFolded: Boolean = {
      link != null || linkedBy.nonEmpty
    }

    def ancestors: Iterator[PptNode] = Iterator.iterate(this.parent)(_.parent).takeWhile(_ != null)

  }

  case class PptLink(to: PptNode, subst: Substitution)

}
