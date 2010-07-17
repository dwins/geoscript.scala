<?xml version="1.0" encoding="UTF-8"?>
<sld:UserStyle xmlns="http://www.opengis.net/sld" xmlns:sld="http://www.opengis.net/sld" xmlns:ogc="http://www.opengis.net/ogc" xmlns:gml="http://www.opengis.net/gml">
    <sld:Name>Default Styler</sld:Name>
    <sld:Title/>
    <sld:FeatureTypeStyle>
        <sld:Name>name</sld:Name>
        <sld:Rule>
            <sld:PolygonSymbolizer>
                <sld:Fill>
                    <sld:GraphicFill>
                        <sld:Graphic>
                            <sld:Mark>
                                <sld:WellKnownName>shape://slash</sld:WellKnownName>
                                <sld:Fill>
                                    <sld:CssParameter name="fill">#ff0000</sld:CssParameter>
                                </sld:Fill>
                                <sld:Stroke>
                                    <sld:CssParameter name="stroke">#008000</sld:CssParameter>
                                </sld:Stroke>
                            </sld:Mark>
                        </sld:Graphic>
                    </sld:GraphicFill>
                </sld:Fill>
            </sld:PolygonSymbolizer>
            <sld:PointSymbolizer>
                <sld:Graphic>
                    <sld:Mark>
                        <sld:WellKnownName>x</sld:WellKnownName>
                        <sld:Fill>
                            <sld:CssParameter name="fill">#ff0000</sld:CssParameter>
                        </sld:Fill>
                        <sld:Stroke>
                            <sld:CssParameter name="stroke">#008000</sld:CssParameter>
                        </sld:Stroke>
                    </sld:Mark>
                    <sld:Size>
                        <ogc:Literal>16</ogc:Literal>
                    </sld:Size>
                </sld:Graphic>
            </sld:PointSymbolizer>
        </sld:Rule>
    </sld:FeatureTypeStyle>
</sld:UserStyle>
