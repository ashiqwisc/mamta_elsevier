find.node.pos.order <- function(set){
  node.pos.matrix <- set$rotation$nodes[,c("code", "MR1", "SVD2")]
  node.pos.matrix$theta <- 0
  for(this.code in node.pos.matrix$code){
    # print(this.code)
    base.vect <- c(1,0)
    this.vect <- c(node.pos.matrix[node.pos.matrix$code == this.code,]$SVD1,	
                   node.pos.matrix[node.pos.matrix$code == this.code,]$SVD2)
    this.theta <- acos( sum(base.vect*this.vect) / ( sqrt(sum(base.vect * base.vect)) * sqrt(sum(this.vect * this.vect)) ) )
    if(this.vect[2] < 0){
      this.theta <- 2*pi-this.theta
    }
    node.pos.matrix$theta[node.pos.matrix$code == this.code] <- this.theta
    node.pos.matrix <- node.pos.matrix[order(theta)]
  }
  return(node.pos.matrix$code)
}

unit.circle.equal.space <- function(set, node_num, radius){
  theta = 0.1
  dTheta = 2*pi / node_num
  for (this.code in find.node.pos.order(set)) {
    set$rotation$nodes[set$rotation$nodes$code == this.code,"MR1"] = radius*cos(theta)
    set$rotation$nodes[set$rotation$nodes$code == this.code,"SVD2"] = radius*sin(theta)
    theta = theta + dTheta
  }
  # for (i in 1:nrow(set$rotation$nodes)) {
  #   set$rotation$nodes[i,2] = R*cos(theta)
  #   set$rotation$nodes[i,3] = R*sin(theta)
  #   theta = theta + dTheta
  # }
  return(set)
}
