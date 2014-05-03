// LSL script generated - patched Render.hs: lslforge.testing.data.scripts.3d_radar_rezzer.lslp Thu Mar 27 23:05:47 Mitteleuropäische Zeit 2014
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

integer Scan = 1;
string avKey;
integer list_pos;
list key_list;
integer key_chan;
default {

	state_entry() {
        llSetObjectName("3D Radar");
    }

	touch_start(integer total_number) {
        if (Scan) {
            llSensorRepeat("","",1,96,3.14159265,1);
            key_list = [];
            llListen(-49222879,"","","");
            llOwnerSay("on");
            Scan = 0;
        }
        else  {
            llSensorRemove();
            llRegionSay(-9423753,"die");
            llOwnerSay("off");
            Scan = 1;
        }
    }

	sensor(integer iNum) {
        integer p = 0;
        for (p = 0; p < iNum; ++p) {
            avKey = llDetectedKey(p);
            list_pos = llListFindList(key_list,[avKey]);
            if (list_pos == -1) {
                key_list = (key_list = []) + key_list + [avKey];
                key_chan = (integer)llFrand(-1000000) - 1000000;
                llRezObject("scan ball",llGetPos(),ZERO_VECTOR,ZERO_ROTATION,key_chan);
                llSleep(0.25);
                llRegionSay(key_chan,avKey);
            }
        }
    }

	listen(integer c,string name,key id,string msg) {
        key remove_key = msg;
        integer r = llListFindList(key_list,[remove_key]);
        llDeleteSubList(key_list,r,r);
    }
}
