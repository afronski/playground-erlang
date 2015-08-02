{:ok, ctx} = :czmq.start_link()

socket = :czmq.zsocket_new(ctx, :czmq_const.zmq_pair)
{:ok, _port} = :czmq.zsocket_bind(socket, "tcp://*:5555")

{:ok, msg} = :czmq.zstr_recv(socket, [{:timeout, 5000}])
IO.inspect(msg)
