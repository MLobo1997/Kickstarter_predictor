import org.apache.commons.csv.CSVRecord;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.SortedMap;
import java.util.TreeMap;

public class DateCount {
    private static final SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd");
    public final TreeMap<Date, Counter> tree = new TreeMap<>();

    DateCount(Iterable<CSVRecord> records) {
        Date launched;
        try {
            for (CSVRecord record : records) {
                launched = df.parse(record.get("launched"));
                tree.put(launched, new Counter());
            }
        } catch (ParseException e) {
            e.printStackTrace();
        }
    }

    void increment (Date launched, Date deadline) {
        SortedMap<Date, Counter> subtree = tree.subMap(launched, true, deadline, true);

        subtree.forEach((date, counter) -> counter.increment());
    }


    @Override
    public String toString() {
        return tree.toString();
    }
}
