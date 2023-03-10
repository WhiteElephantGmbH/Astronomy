-- *********************************************************************************************************************
-- *                           (c) 2023 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************

MODULE TimeServer;

IMPORT Main, Node;
<*WOFF306+*>
IMPORT Network, Observe_;
<*WOFF306-*>

CONST
  Nodes = Node.All;

BEGIN
  Main.Initialize;
END TimeServer.
