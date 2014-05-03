// LSL script generated - patched Render.hs: lslforge.testing.data.scripts.3d_radar_scan_ball.lslp Thu Mar 27 23:05:47 Mitteleuropäische Zeit 2014
// ATTRIBUTION_BEGIN
// This work uses content from the Second Life� Wiki article:
// http://wiki.secondlife.com/wiki/3D_Radar
// Copyright � 2008 Linden Research, Inc. 
// Author: Jesse Barnett, others
// Licensed under the Creative Commons Attribution-Share Alike 3.0 License:
// http://creativecommons.org/licenses/by-sa/3.0
// See the complete license terms:
// http://creativecommons.org/licenses/by-sa/3.0/legalcode
// ATTRIBUTION_END

string avName;
string avDistance;
key avKey;
integer avListen;
integer key_chan;
vector avPos;
vector rPos;
default {

	state_entry() {
        llSetObjectName("scan ball");
    }

	on_rez(integer start_param) {
        rPos = llGetPos();
        key_chan = start_param;
        llListen(-9423753,"","","");
        avListen = llListen(key_chan,"","","");
    }

	listen(integer c,string n,key id,string msg) {
        if (c == -9423753) llDie();
        else  {
            avKey = (key)msg;
            llSensorRepeat("",avKey,1,96,3.14159265,1.0);
            llListenRemove(avListen);
        }
    }

	sensor(integer n) {
        avPos = llDetectedPos(0);
        integer name = 1;
        if (name) {
            avName = llDetectedName(0);
            name = 0;
        }
        vector avDivPos = (avPos - rPos) * 1.0417e-2;
        avDistance = (string)llVecDist(rPos,llDetectedPos(0));
        llSetPos(rPos + avDivPos);
        llSetText(avName + "[" + avDistance + "]",<1.0,1.0,1.0>,1);
    }

	no_sensor() {
        llRegionSay(-49222879,avKey);
        llOwnerSay(avName + " is now out of range.");
        llDie();
    }
}
