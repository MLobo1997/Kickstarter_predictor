public class Counter {
    private Integer nr;

    public Counter() {
        nr = 0;
    }

    public Integer increment (){
        nr++;
        return nr;
    }

    public Integer getNr() {
        return nr;
    }

    @Override
    public String toString() {
        return nr.toString();
    }
}
