provider "aws" {
    region = "eu-west-1"
    profile = "leastauthority-staging"
}

/*
 * Centralize configuration about which VPC we're operating in.
 */
data "aws_vpc" "main" {
  id = "vpc-76dfc012"
}

/*
 * Create a security group that allows all traffic on everything.
 * TODO: Tighten this up.
 */
resource "aws_security_group" "allow_all" {
  name        = "allow_all"
  description = "Allow all inbound traffic"
  vpc_id      = "${data.aws_vpc.main.id}"

  ingress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  egress {
    from_port       = 0
    to_port         = 0
    protocol        = "-1"
    cidr_blocks     = ["0.0.0.0/0"]
  }
}
