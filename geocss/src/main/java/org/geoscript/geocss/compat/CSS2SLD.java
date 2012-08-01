package org.geoscript.geocss.compat;

import java.io.Reader;
import java.net.URL;
import org.geotools.styling.Style;

public final class CSS2SLD {
    protected static final Converter impl = new Converter();

    private CSS2SLD() throws Exception {
        throw new Exception("You shouldn't be instantiating this class");
    }

    public static Style convert(Reader input) {
        return impl.convert(input);
    }

    public static Style convert(Reader input, URL baseURL) {
        return impl.convert(input, baseURL);
    }
}
