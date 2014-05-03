// LSL script generated - patched Render.hs: lslforge.eclipse.lslforge.samples.dialog_example.dialog_script.lslp Thu Mar 27 23:05:47 Mitteleuropäische Zeit 2014


default {

    state_entry() {
        llSensor("Default Avatar",NULL_KEY,1,96.0,3.14159265);
    }

    
    sensor(integer num_detected) {
        key k = llDetectedKey(0);
        llListen(-12345,"",NULL_KEY,"");
        llDialog(k,"hello",["Not Ok","Ok"],-12345);
    }

    
    no_sensor() {
        llOwnerSay("couldn't find Default Avatar to talk to");
    }

    
    listen(integer channel,string name,key id,string message) {
        llOwnerSay("on channel " + (string)channel + ", received message \"" + message + "\" from " + name);
    }
}
