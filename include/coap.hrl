-record(coap_message, {type, method, id, token = <<>>, options = [], payload = <<>>}).

-record(coap_resource, {etag, format, content = <<>>}).

-type coap_message() :: #coap_message{}.
