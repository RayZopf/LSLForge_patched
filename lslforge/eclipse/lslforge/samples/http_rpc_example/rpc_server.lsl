// LSL script generated - patched Render.hs: lslforge.eclipse.lslforge.samples.http_rpc_example.rpc_server.lslp Thu Mar 27 23:05:47 Mitteleuropäische Zeit 2014

key rpcChannel = NULL_KEY;
default {

    state_entry() {
        llOpenRemoteDataChannel();
        llOwnerSay("opening remote data channel");
    }

    
    remote_data(integer event_type,key channel,key message_id,string sender,integer idata,string sdata) {
        llOwnerSay("got remote data event");
        if (event_type == 1) {
            rpcChannel = channel;
            llHTTPRequest("http://www.example.com/post_channel",[0,"POST"],(string)channel);
        }
        else  if (event_type == 2) {
            llOwnerSay("recieved request: " + sdata + ", " + (string)idata);
            llRemoteDataReply(channel,message_id,"",0);
        }
    }

    
    http_response(key request_id,integer status,list metadata,string body) {
        llOwnerSay("Registration of channel successful!");
    }
}
