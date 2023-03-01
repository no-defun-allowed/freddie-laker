# Freddie Laker

Enough of a Discord API to run [Bakerposting](https://github.com/no-defun-allowed/Bakerposting)
and no more. Trying to keep it reliable enough to run on a server with
shaky connections without manual intervention.

No threads are created or destroyed outside websocket-driver -- speaking of, use
[this fork](https://github.com/no-defun-allowed/websocket-driver) of it.
Things are scheduled using mailboxes and timeouts.
