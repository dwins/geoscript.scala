package org.geoscript.geocss.compat;

import java.io.InputStream;
import org.geotools.styling.Style;

public final class CSS2SLD {
    static abstract class Converter {
        public abstract Style convert(InputStream input);
    }

    protected static Converter impl;

    static {
        impl = new Impl();
    }

    private CSS2SLD() throws Exception {
        throw new Exception("You shouldn't be instantiating this class");
    }

    public Style convert(InputStream input) {
        return impl.convert(input);
    }
}
