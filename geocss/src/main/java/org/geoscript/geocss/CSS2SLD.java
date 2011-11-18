package org.geoscript.geocss.compat;

import java.io.Reader;
import java.net.URL;
import org.geotools.styling.Style;

public final class CSS2SLD {
    static abstract class Converter {
        public abstract Style convert(Reader input);
        public abstract Style convert(Reader input, URL baseURL);
    }

    protected static Converter impl;

    static {
        impl = new Impl();
    }

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
