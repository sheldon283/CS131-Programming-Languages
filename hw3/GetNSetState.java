import java.util.concurrent.atomic.AtomicIntegerArray;

class GetNSetState implements State {
    private AtomicIntegerArray value;
    private byte maxval;    

    GetNSetState(byte[] v) { setValue(v); maxval = 127; }

    GetNSetState(byte[] v, byte m) { setValue(v); maxval = m; }

    public int size() { return value.length(); }

    public byte[] current() { 
        byte[] allbytes = new byte[value.length()];

        int i = 0;
        while (i < value.length()) {
            allbytes[i] = (byte) value.get(i);
            i++;
        }

        return allbytes;
    }

    private void setValue(byte[] v){
        int length = v.length;
        int[] arr = new int[length];

        int i = 0;
        while (i < length) {
            arr[i] = v[i];
            i++;
        }

        value = new AtomicIntegerArray(arr);

    }

    public boolean swap(int i, int j) {
	    if (value.get(i) <= 0 || value.get(j) >= maxval) {
	        return false;
        }
        
	    value.getAndDecrement(i);
	    value.getAndIncrement(j);
	    return true;
    }
}