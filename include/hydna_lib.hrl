-define(HEARTBEAT, 0).
-define(OPEN, 1).
-define(DATA, 2).
-define(EMIT, 3).
-define(RSLV, 4).

-define(OPEN_OK, 0).
-define(OPEN_REDIRECT, 1).
-define(OPEN_DENY, 3).

-define(EMIT_SIGNAL, 0).
-define(EMIT_END, 1).

-define(MODE_LISTEN, 0).
-define(MODE_READ, 1).
-define(MODE_WRITE, 2).
-define(MODE_EMIT, 4).

-define(PAYLOAD_MAX_LENGTH, 16#FFFA).
