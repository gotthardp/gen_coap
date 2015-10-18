# Generic Erlang CoAP Client/Server

Pure Erlang implementation of the Constrained Application Protocol (CoAP),
which aims to be conformant with:
 - CoAP core protocol [RFC 7252](https://tools.ietf.org/rfc/rfc7252.txt)
 - CoAP Observe option [RFC 7641](https://tools.ietf.org/rfc/rfc7641.txt)
 - Block-wise transfers in CoAP [draft-ietf-core-block-18](https://tools.ietf.org/id/draft-ietf-core-block-18.txt)
 - CoRE link format [RFC 6690](https://tools.ietf.org/rfc/rfc6690.txt)

Tested with the following CoAP implementations:
 - C [libcoap](https://www.libcoap.net/) (develop branch)
 - [coap.me](http://coap.me/)

Used in the following applications:
 - [CoAP Publish-Subscribe interface to RabbitMQ](https://github.com/gotthardp/rabbitmq-coap-pubsub)

Let me know if you (intend to) use *gen_coap*. The API may change and some
functions may not be implemented. Please
[add an Issue](https://github.com/gotthardp/gen_coap/issues/new)
if you find a bug or miss a feature.


## Usage
*gen_coap* enables you to integrate a CoAP server and/or CoAP client with
your application. For demonstration purposes it also includes a simple CoAP
client and server implemented using [escript](http://www.erlang.org/doc/man/escript.html).

[![Build Status](https://travis-ci.org/gotthardp/gen_coap.svg?branch=master)](https://travis-ci.org/gotthardp/gen_coap)

### Client
Have a look at [coap-client.sh](coap-client.sh). It implements a simple
command line utility for manipulation and observation of CoAP resources. It
shall demonstrate the use of the `coap_client` and `coap_observer` modules.
The tool accepts the following arguments:

 Argument      | Description
---------------|---------------
 -m *Method*   | request method (get, put, post or delete), default is 'get'
 -e *Text*     | include text as payload
 -s *Duration* | subscribe for given duration [seconds]
 *Uri*         | coap:// URI identifying the resource

Run the example simply by:

    $ ./coap-client.sh coap://127.0.0.1/.well-known/core
    $ ./coap-client.sh coap://127.0.0.1/resource
    $ ./coap-client.sh coap://127.0.0.1/resource -s 1000
    $ ./coap-client.sh -m put coap://127.0.0.1/resource -e data
    $ ./coap-client.sh -m delete coap://127.0.0.1/resource

In an erlang program you can get a CoAP resource by:
```erlang
{ok, content, Data} = coap_client:request(get, "coap://coap.me:5683")
```
No application need to be started to use the client.

### Server
Have a look at [coap-server.sh](coap-server.sh). It implements a simple
resource storage, which can be accessed using CoAP. It shall demonstrate the
use of the `coap_server_registry` and `coap_responder` modules. The entire
server is implemented using [escript](http://www.erlang.org/doc/man/escript.html)
and requires no arguments. Run the sample server as follows and then access
it using any CoAP client (or the *gen_coap* sample client tool):

    $ ./coap-server.sh

You can manually start the server from the Erlang command line by:

    $ erl -pa ebin
    1> application:start(gen_coap).

However, the server out of a box does not offer any resources. To offer CoAP access
to some server resources you need to implement the `coap_resource` behaviour,
which defines callbacks that the server invokes upon reception of a CoAP request.
 - `coap_discover` is called when a CoAP client asks for the list of
   ".well-known/core" resources.
 - `coap_get`, `coap_post`, `coap_put` or `coap_delete` is called when the server
   receives a GET, POST, PUT or DELETE request for a resource.
 - `coap_observe` or `coap_unobserve` is called upon a GET request with an
   Observe=0 or Observe=1 option.

### Architecture

The following picture shows the `gen_coap` modules are their relationships:
![GitHub Logo](https://rawgit.com/gotthardp/gen_coap/master/doc/architecture.svg)
