{:ok, ctx} = :czmq.start_link()
socket = :czmq.zsocket_new(ctx, :czmq_const.zmq_pair)

:ok = :czmq.zsocket_connect(socket, "tcp://localhost:5555")
:ok = :czmq.zstr_send(socket, "Hello Server!")
