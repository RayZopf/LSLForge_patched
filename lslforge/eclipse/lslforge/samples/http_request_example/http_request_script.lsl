// LSL script generated - patched Render.hs: lslforge.eclipse.lslforge.samples.http_request_example.http_request_script.lslp Thu Mar 27 23:05:47 Mitteleuropäische Zeit 2014

key k = NULL_KEY;
default {

    state_entry() {
        k = llRequestURL();
    }

    
    http_request(key id,string method,string body) {
        if (method == "URL_REQUEST_GRANTED") {
            llOwnerSay(body);
        }
        else  if (method == "URL_REQUEST_DENIED") {
            llSay(0,"Couldn't get an URL!");
        }
        else  if (method == "GET") {
            llSay(0,"x-path-info: " + llGetHTTPHeader(id,"x-path-info") + ", x-query-string: " + llGetHTTPHeader(id,"x-query-string") + ", x-remote-ip: " + llGetHTTPHeader(id,"x-remote-ip") + ", user-agent: " + llGetHTTPHeader(id,"user-agent"));
            llSay(0,"free-urls: " + (string)llGetFreeURLs());
            llHTTPResponse(id,200,"ok");
        }
    }
}
