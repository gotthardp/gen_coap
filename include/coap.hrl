-record(coap_message, {type, method, id, token = <<>>, options = [], payload = <<>>}).

-type coap_message() :: #coap_message{}.
