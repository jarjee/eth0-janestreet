import json

__hasConnected = False;
__team_name = "CARBONFOURTEEN"
__exchange_ip = "10.0.131.184"

def connectionCheck():
    if (! __hasConnected):
        print("Have not connected to the server yet!")
        exit(1)

def connect():
    #Send {"type": "hello", "team": "CARBONFOURTEEN"}
    __hasConnected = True;
    pass

#With an already established connection, add an order
def add(order_id, com, price, size):
    #Symbol is team name
    connectionCheck()
    #Send {"type": "add", "order_id": N, "symbol": "SYM", "dir": "BUY", "price": N, "size": N}
    pass

#Converting an order from buy to sell, change size, symbol etc
def convert(order_id, com, size):
    #Symbol is team name
    connectionCheck()
    #Send {"type": "convert", "order_id": N, "symbol": "SYM", "dir": "BUY", "size": N}
    pass

#Cancel an order which we have an id for
def cancel(order_id):
    connectionCheck()
    #Send {"type": "cancel", "order_id": N}
    pass
