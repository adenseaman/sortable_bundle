%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This header file contains definitions used by the cluster software
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% unique software ID name used to generate global names for clients and servers
-define(SOFTWAREID,distmap).

% number of chunks that server requests from client at a time
-define(DEFAULT_NUM_CHUNKS,10).
%-define(DEFAULT_NUM_CHUNKS,2).

% number of data elements that client coordinator puts in a chunk
-define(DEFAULT_CHUNK_SIZE,1000).
%-define(DEFAULT_CHUNK_SIZE,2).
