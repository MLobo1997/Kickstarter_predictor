import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;
import org.apache.commons.csv.CSVRecord;

import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Reader;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Iterator;

public class Main {
    private static final SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd");
    private static final String path = "../datasets/ks-projects-201801.csv";

    private static Iterable<CSVRecord> parser(String path){
        try {
            Reader in = new FileReader(path);
            return CSVFormat.EXCEL.withFirstRecordAsHeader().parse(in);

        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }


    public static void main(String [] args) {
        Iterable<CSVRecord> records = parser(path);

        DateCount dateCount = new DateCount(records);

        records = parser(path);

        Date launched, deadline;

        float i = 0;
        try {
            assert records != null;
            for (CSVRecord record : records) {
                launched = df.parse(record.get("launched"));
                deadline = df.parse(record.get("deadline"));
                dateCount.increment(launched, deadline);

                i++;
                System.out.println(i/378621 * 100);
            }
        } catch (ParseException e) {
            e.printStackTrace();
        }

        records = parser(path);
        try {
            CSVPrinter printer = new CSVPrinter(new FileWriter("2018_WithOtherActive.csv"), CSVFormat.EXCEL);
            printer.printRecord("ID","name","category","main_category","currency","deadline","goal","launched","pledged","state","backers","country","usd_pledged","usd_pledged_real","usd_goal_real", "other_active_projects");
            Iterator<String> elemRec;


            assert records != null;
            for (CSVRecord record : records) {
                elemRec = record.iterator();

                while (elemRec.hasNext())
                    printer.print(elemRec.next());

                printer.print(dateCount.tree.get(df.parse(record.get("launched"))).getNr());

                printer.println();
            }

            printer.flush();
            printer.close();

        } catch (IOException | ParseException e) {
            e.printStackTrace();
        }
    }
}
