import zmq
import json

def format_json(raw):
    json_obj = json.loads(raw)

    formatting_opts = {
        'sort_keys': True,
        'indent': 4,
        'separators': (',', ': ')
    }

    return json.dumps(json_obj, **formatting_opts)

def init_socket(ctx):
    socket = ctx.socket(zmq.REP)
    socket.bind("tcp://*:5555")

    print("Listening on port 5555")

    return socket

def handle_msg(msg, socket):
    print("Got %s" % msg)

    try:
        socket.send("+%s" % format_json(msg))
    except Exception as e:
        socket.send("-%s" % e)

def start():
    ctx = zmq.Context()
    socket = init_socket(ctx)

    while True:
        handle_msg(socket.recv(), socket)

if __name__ == "__main__":
    start()
