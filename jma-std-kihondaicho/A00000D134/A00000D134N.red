<?xml version="1.0" encoding="UTF-8"?>
<dia:diagram xmlns:dia="http://www.lysator.liu.se/~alla/dia/">
  <dia:dictionarydata>
    <element name="TITLE" occurs="1">
      <element name="SUBTITLE" occurs="1">
        <appinfo>
          <embed object="text" length="30"/>
        </appinfo>
      </element>
      <element name="JOBMSG" occurs="1">
        <appinfo>
          <embed object="text" length="60"/>
        </appinfo>
      </element>
      <element name="PAGE" occurs="1">
        <appinfo>
          <embed object="text" length="3"/>
        </appinfo>
      </element>
      <element name="SYSYMD" occurs="1">
        <appinfo>
          <embed object="text" length="22"/>
        </appinfo>
      </element>
      <element name="FROMYMD" occurs="1">
        <appinfo>
          <embed object="text" length="22"/>
        </appinfo>
      </element>
      <element name="TOYMD" occurs="1">
        <appinfo>
          <embed object="text" length="22"/>
        </appinfo>
      </element>
      <element name="SRYKA" occurs="1">
        <appinfo>
          <embed object="text" length="20"/>
        </appinfo>
      </element>
      <element name="DRNAME" occurs="1">
        <appinfo>
          <embed object="text" length="30"/>
        </appinfo>
      </element>
      <element name="TENKI-G" occurs="3">
        <element name="TENKI" occurs="1">
          <appinfo>
            <embed object="text" length="22"/>
          </appinfo>
        </element>
      </element>
    </element>
    <element name="DATA" occurs="1">
      <element name="TDAY-G" occurs="15">
        <element name="TDAY" occurs="1">
          <appinfo>
            <embed object="text" length="10"/>
          </appinfo>
        </element>
      </element>
      <element name="YOSO-G" occurs="15">
        <element name="YOSO-OCC" occurs="38">
          <element name="YOSO" occurs="1">
            <appinfo>
              <embed object="text" length="11"/>
            </appinfo>
          </element>
        </element>
      </element>
    </element>
  </dia:dictionarydata>
  <dia:diagramdata>
    <dia:attribute name="background">
      <dia:color val="#ffffff"/>
    </dia:attribute>
    <dia:attribute name="pagebreak">
      <dia:color val="#000099"/>
    </dia:attribute>
    <dia:attribute name="paper">
      <dia:composite type="paper">
        <dia:attribute name="name">
          <dia:string>#A4#</dia:string>
        </dia:attribute>
        <dia:attribute name="tmargin">
          <dia:real val="0"/>
        </dia:attribute>
        <dia:attribute name="bmargin">
          <dia:real val="0"/>
        </dia:attribute>
        <dia:attribute name="lmargin">
          <dia:real val="0"/>
        </dia:attribute>
        <dia:attribute name="rmargin">
          <dia:real val="0"/>
        </dia:attribute>
        <dia:attribute name="is_portrait">
          <dia:boolean val="false"/>
        </dia:attribute>
        <dia:attribute name="scaling">
          <dia:real val="1"/>
        </dia:attribute>
        <dia:attribute name="fitto">
          <dia:boolean val="false"/>
        </dia:attribute>
        <dia:attribute name="custom_width">
          <dia:real val="10"/>
        </dia:attribute>
        <dia:attribute name="custom_height">
          <dia:real val="10"/>
        </dia:attribute>
      </dia:composite>
    </dia:attribute>
    <dia:attribute name="grid">
      <dia:composite type="grid">
        <dia:attribute name="width_x">
          <dia:real val="0.05000000074505806"/>
        </dia:attribute>
        <dia:attribute name="width_y">
          <dia:real val="0.10000000149011612"/>
        </dia:attribute>
        <dia:attribute name="visible_x">
          <dia:int val="1"/>
        </dia:attribute>
        <dia:attribute name="visible_y">
          <dia:int val="1"/>
        </dia:attribute>
        <dia:composite type="color"/>
      </dia:composite>
    </dia:attribute>
    <dia:attribute name="color">
      <dia:color val="#d8e5e5"/>
    </dia:attribute>
    <dia:attribute name="guides">
      <dia:composite type="guides">
        <dia:attribute name="hguides"/>
        <dia:attribute name="vguides"/>
      </dia:composite>
    </dia:attribute>
  </dia:diagramdata>
  <dia:layer name="線" visible="true">
    <dia:object type="Standard - Line" version="0" id="O0">
      <dia:attribute name="obj_pos">
        <dia:point val="1,3.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.99,3.69;29.01,3.71"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,3.7"/>
        <dia:point val="29,3.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O1">
      <dia:attribute name="obj_pos">
        <dia:point val="5.2,3.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.19,3.29;5.21044,18.9091"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.2,3.3"/>
        <dia:point val="5.20044,18.8991"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="28"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O2">
      <dia:attribute name="obj_pos">
        <dia:point val="6.9,3.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.9,3.3;6.9,18.9"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="6.9,3.3"/>
        <dia:point val="6.9,18.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="28"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O3">
      <dia:attribute name="obj_pos">
        <dia:point val="2.1,4.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.1,4.9;29,4.9"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.1,4.9"/>
        <dia:point val="29,4.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O4">
      <dia:attribute name="obj_pos">
        <dia:point val="1,6.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.99,6.89;29.01,6.91"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,6.9"/>
        <dia:point val="29,6.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O5">
      <dia:attribute name="obj_pos">
        <dia:point val="1,5.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.99,5.69;29.01,5.71"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,5.7"/>
        <dia:point val="29,5.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O6">
      <dia:attribute name="obj_pos">
        <dia:point val="1,7.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1,7.7;29,7.7"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,7.7"/>
        <dia:point val="29,7.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O7">
      <dia:attribute name="obj_pos">
        <dia:point val="1,8.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1,8.9;29,8.9"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,8.9"/>
        <dia:point val="29,8.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O8">
      <dia:attribute name="obj_pos">
        <dia:point val="1,9.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1,9.3;29,9.3"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,9.3"/>
        <dia:point val="29,9.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O9">
      <dia:attribute name="obj_pos">
        <dia:point val="2.1,4.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.1,4.5;29,4.5"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.1,4.5"/>
        <dia:point val="29,4.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O10">
      <dia:attribute name="obj_pos">
        <dia:point val="2.1,5.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.1,5.3;29,5.3"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.1,5.3"/>
        <dia:point val="29,5.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O11">
      <dia:attribute name="obj_pos">
        <dia:point val="1,7.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1,7.3;29,7.3"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,7.3"/>
        <dia:point val="29,7.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O12">
      <dia:attribute name="obj_pos">
        <dia:point val="1,8.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1,8.5;29,8.5"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,8.5"/>
        <dia:point val="29,8.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O13">
      <dia:attribute name="obj_pos">
        <dia:point val="1,8.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1,8.1;29,8.1"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,8.1"/>
        <dia:point val="29,8.1"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O14">
      <dia:attribute name="obj_pos">
        <dia:point val="1,9.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1,9.7;29,9.7"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,9.7"/>
        <dia:point val="29,9.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O15">
      <dia:attribute name="obj_pos">
        <dia:point val="1,18.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.99,18.89;29.01,18.91"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,18.9"/>
        <dia:point val="29,18.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O16">
      <dia:attribute name="obj_pos">
        <dia:point val="2.1,4.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.1,4.1;29,4.1"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.1,4.1"/>
        <dia:point val="29,4.1"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O17">
      <dia:attribute name="obj_pos">
        <dia:point val="8.6,3.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.6,3.3;8.6,18.9"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8.6,3.3"/>
        <dia:point val="8.6,18.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="28"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O18">
      <dia:attribute name="obj_pos">
        <dia:point val="1,3.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.99,3.29;29.01,3.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,3.3"/>
        <dia:point val="29,3.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O19">
      <dia:attribute name="obj_pos">
        <dia:point val="10.3,3.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.3,3.3;10.3,18.9"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10.3,3.3"/>
        <dia:point val="10.3,18.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="28"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O20">
      <dia:attribute name="obj_pos">
        <dia:point val="12,3.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12,3.3;12,18.9"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="12,3.3"/>
        <dia:point val="12,18.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="28"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O21">
      <dia:attribute name="obj_pos">
        <dia:point val="13.7,3.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.7,3.3;13.7,18.9"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.7,3.3"/>
        <dia:point val="13.7,18.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="28"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O22">
      <dia:attribute name="obj_pos">
        <dia:point val="22.2,3.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.2,3.3;22.2,18.9"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="22.2,3.3"/>
        <dia:point val="22.2,18.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="28"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O23">
      <dia:attribute name="obj_pos">
        <dia:point val="15.4,3.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.4,3.3;15.4,18.9"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.4,3.3"/>
        <dia:point val="15.4,18.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="28"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O24">
      <dia:attribute name="obj_pos">
        <dia:point val="29,3.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="28.99,3.29;29.01,18.91"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="29,3.3"/>
        <dia:point val="29,18.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O25">
      <dia:attribute name="obj_pos">
        <dia:point val="1,10.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1,10.1;29,10.1"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,10.1"/>
        <dia:point val="29,10.1"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O26">
      <dia:attribute name="obj_pos">
        <dia:point val="17.1,3.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1,3.3;17.1,18.9"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.1,3.3"/>
        <dia:point val="17.1,18.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="28"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O27">
      <dia:attribute name="obj_pos">
        <dia:point val="1,6.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1,6.5;29,6.5"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,6.5"/>
        <dia:point val="29,6.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O28">
      <dia:attribute name="obj_pos">
        <dia:point val="1,10.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1,10.5;29,10.5"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,10.5"/>
        <dia:point val="29,10.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O29">
      <dia:attribute name="obj_pos">
        <dia:point val="1,10.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1,10.9;29,10.9"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,10.9"/>
        <dia:point val="29,10.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O30">
      <dia:attribute name="obj_pos">
        <dia:point val="1,11.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1,11.3;29,11.3"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,11.3"/>
        <dia:point val="29,11.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O31">
      <dia:attribute name="obj_pos">
        <dia:point val="2.1,13.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.1,13.3;29,13.3"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.1,13.3"/>
        <dia:point val="29,13.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O32">
      <dia:attribute name="obj_pos">
        <dia:point val="2.1,13.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.1,13.7;29,13.7"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.1,13.7"/>
        <dia:point val="29,13.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O33">
      <dia:attribute name="obj_pos">
        <dia:point val="2.1,14.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.1,14.1;29,14.1"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.1,14.1"/>
        <dia:point val="29,14.1"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O34">
      <dia:attribute name="obj_pos">
        <dia:point val="2.1,14.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.1,14.5;29,14.5"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.1,14.5"/>
        <dia:point val="29,14.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O35">
      <dia:attribute name="obj_pos">
        <dia:point val="2.1,14.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.1,14.9;29,14.9"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.1,14.9"/>
        <dia:point val="29,14.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O36">
      <dia:attribute name="obj_pos">
        <dia:point val="2.1,15.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.1,15.3;29,15.3"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.1,15.3"/>
        <dia:point val="29,15.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O37">
      <dia:attribute name="obj_pos">
        <dia:point val="2.1,15.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.1,15.7;29,15.7"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.1,15.7"/>
        <dia:point val="29,15.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O38">
      <dia:attribute name="obj_pos">
        <dia:point val="2.1,16.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.1,16.1;29,16.1"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.1,16.1"/>
        <dia:point val="29,16.1"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O39">
      <dia:attribute name="obj_pos">
        <dia:point val="2.1,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.1,16.9;29,16.9"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.1,16.9"/>
        <dia:point val="29,16.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O40">
      <dia:attribute name="obj_pos">
        <dia:point val="2.1,16.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.1,16.5;29,16.5"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.1,16.5"/>
        <dia:point val="29,16.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O41">
      <dia:attribute name="obj_pos">
        <dia:point val="2.1,17.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.1,17.3;29,17.3"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.1,17.3"/>
        <dia:point val="29,17.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O42">
      <dia:attribute name="obj_pos">
        <dia:point val="2.1,17.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.1,17.7;29,17.7"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.1,17.7"/>
        <dia:point val="29,17.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O43">
      <dia:attribute name="obj_pos">
        <dia:point val="2.1,18.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.1,18.1;29,18.1"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.1,18.1"/>
        <dia:point val="29,18.1"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O44">
      <dia:attribute name="obj_pos">
        <dia:point val="18.8,3.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.8,3.3;18.8,18.9"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="18.8,3.3"/>
        <dia:point val="18.8,18.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="28"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O45">
      <dia:attribute name="obj_pos">
        <dia:point val="20.5,3.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.5,3.3;20.5,18.9"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="20.5,3.3"/>
        <dia:point val="20.5,18.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="28"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O46">
      <dia:attribute name="obj_pos">
        <dia:point val="23.9,3.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="23.9,3.3;23.9,18.9"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="23.9,3.3"/>
        <dia:point val="23.9,18.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="28"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O47">
      <dia:attribute name="obj_pos">
        <dia:point val="10,1.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.98,1.18;21.02,1.22"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10,1.2"/>
        <dia:point val="21,1.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.040000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O48">
      <dia:attribute name="obj_pos">
        <dia:point val="10,1.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.98,1.28;21.02,1.32"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10,1.3"/>
        <dia:point val="21,1.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.040000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O49">
      <dia:attribute name="obj_pos">
        <dia:point val="1,13.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1,13.3;1.5,13.3"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,13.3"/>
        <dia:point val="1.5,13.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O50">
      <dia:attribute name="obj_pos">
        <dia:point val="1,13.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1,13.7;1.5,13.7"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,13.7"/>
        <dia:point val="1.5,13.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O51">
      <dia:attribute name="obj_pos">
        <dia:point val="1,14.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1,14.1;1.5,14.1"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,14.1"/>
        <dia:point val="1.5,14.1"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O52">
      <dia:attribute name="obj_pos">
        <dia:point val="1,14.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1,14.5;1.5,14.5"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,14.5"/>
        <dia:point val="1.5,14.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O53">
      <dia:attribute name="obj_pos">
        <dia:point val="1,14.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1,14.9;1.5,14.9"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,14.9"/>
        <dia:point val="1.5,14.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O54">
      <dia:attribute name="obj_pos">
        <dia:point val="1,15.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1,15.3;1.5,15.3"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,15.3"/>
        <dia:point val="1.5,15.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O55">
      <dia:attribute name="obj_pos">
        <dia:point val="1,15.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1,15.7;1.5,15.7"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,15.7"/>
        <dia:point val="1.5,15.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O56">
      <dia:attribute name="obj_pos">
        <dia:point val="1,16.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1,16.1;1.5,16.1"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,16.1"/>
        <dia:point val="1.5,16.1"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O57">
      <dia:attribute name="obj_pos">
        <dia:point val="1,16.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1,16.5;1.5,16.5"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,16.5"/>
        <dia:point val="1.5,16.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O58">
      <dia:attribute name="obj_pos">
        <dia:point val="1,18.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1,18.1;1.5,18.1"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,18.1"/>
        <dia:point val="1.5,18.1"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O59">
      <dia:attribute name="obj_pos">
        <dia:point val="1,17.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1,17.7;1.5,17.7"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,17.7"/>
        <dia:point val="1.5,17.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O60">
      <dia:attribute name="obj_pos">
        <dia:point val="2.1,3.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.09,3.69;2.11,5.71"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.1,3.7"/>
        <dia:point val="2.1,5.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O61">
      <dia:attribute name="obj_pos">
        <dia:point val="1,3.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.99,3.29;1.01,18.91"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,3.3"/>
        <dia:point val="1,18.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O62">
      <dia:attribute name="obj_pos">
        <dia:point val="2.1,12.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.09,12.49;2.11,18.91"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.1,12.5"/>
        <dia:point val="2.1,18.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O63">
      <dia:attribute name="obj_pos">
        <dia:point val="1.5,3.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.49,3.29;1.51,18.91"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.5,3.3"/>
        <dia:point val="1.5,18.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O64">
      <dia:attribute name="obj_pos">
        <dia:point val="1,5.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1,5.3;1.5,5.3"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,5.3"/>
        <dia:point val="1.5,5.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O65">
      <dia:attribute name="obj_pos">
        <dia:point val="1,4.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1,4.9;1.5,4.9"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,4.9"/>
        <dia:point val="1.5,4.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O66">
      <dia:attribute name="obj_pos">
        <dia:point val="1,4.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1,4.1;1.5,4.1"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,4.1"/>
        <dia:point val="1.5,4.1"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O67">
      <dia:attribute name="obj_pos">
        <dia:point val="1,4.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1,4.5;1.5,4.5"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,4.5"/>
        <dia:point val="1.5,4.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O68">
      <dia:attribute name="obj_pos">
        <dia:point val="1,12.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.99,12.49;29.01,12.51"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,12.5"/>
        <dia:point val="29,12.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O69">
      <dia:attribute name="obj_pos">
        <dia:point val="1,12.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1,12.1;29,12.1"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,12.1"/>
        <dia:point val="29,12.1"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O70">
      <dia:attribute name="obj_pos">
        <dia:point val="1,11.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1,11.7;29,11.7"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,11.7"/>
        <dia:point val="29,11.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O71">
      <dia:attribute name="obj_pos">
        <dia:point val="2.1,12.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.1,12.9;29,12.9"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.1,12.9"/>
        <dia:point val="29,12.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O72">
      <dia:attribute name="obj_pos">
        <dia:point val="1,12.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1,12.9;1.5,12.9"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,12.9"/>
        <dia:point val="1.5,12.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O73">
      <dia:attribute name="obj_pos">
        <dia:point val="1,6.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1,6.1;29,6.1"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,6.1"/>
        <dia:point val="29,6.1"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O74">
      <dia:attribute name="obj_pos">
        <dia:point val="25.6,3.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.6,3.3;25.6,18.9"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="25.6,3.3"/>
        <dia:point val="25.6,18.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="28"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O75">
      <dia:attribute name="obj_pos">
        <dia:point val="27.3,3.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.3,3.3;27.3,18.9"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="27.3,3.3"/>
        <dia:point val="27.3,18.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="28"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O76">
      <dia:attribute name="obj_pos">
        <dia:point val="1,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1,16.9;1.5,16.9"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,16.9"/>
        <dia:point val="1.5,16.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O77">
      <dia:attribute name="obj_pos">
        <dia:point val="1,17.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1,17.3;1.5,17.3"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,17.3"/>
        <dia:point val="1.5,17.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O78">
      <dia:attribute name="obj_pos">
        <dia:point val="1.5,18.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1,18.5;1.5,18.5"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.5,18.5"/>
        <dia:point val="1,18.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O79">
      <dia:attribute name="obj_pos">
        <dia:point val="2.1,18.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.1,18.5;29,18.5"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.1,18.5"/>
        <dia:point val="29,18.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
  </dia:layer>
  <dia:layer name="文字" visible="true">
    <dia:object type="Standard - Text" version="1" id="O80">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,3.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,2.79;2.5,3.1425"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#診療科：#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,3.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O81">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,3.735;1.4,4.0375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ア#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O82">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,4.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,4.135;1.4,4.4375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#イ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,4.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O83">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,4.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,4.535;1.4,4.8375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ウ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,4.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O84">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,5.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,4.935;1.4,5.2375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#エ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,5.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O85">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,5.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,5.335;1.4,5.6375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#オ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,5.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O86">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,5.735;1.4,6.0375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#カ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O87">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,6.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,6.135;1.4,6.4375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#キ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,6.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O88">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,6.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,6.535;1.4,6.8375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ク#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,6.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O89">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,7.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,6.935;1.4,7.2375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ケ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,7.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O90">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,7.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,7.335;1.4,7.6375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#コ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,7.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O91">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,7.735;1.4,8.0375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#サ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O92">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,8.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,8.135;1.4,8.4375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#シ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,8.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O93">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,8.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,8.535;1.4,8.8375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ス#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,8.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O94">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,9.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,8.935;1.4,9.2375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#セ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,9.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O95">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,9.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,9.335;1.4,9.6375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ソ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,9.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O96">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,10"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,9.735;1.4,10.0375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#タ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,10"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O97">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,10.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,10.135;1.4,10.4375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#チ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,10.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O98">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,10.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,10.535;1.4,10.8375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ツ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,10.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O99">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,11.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,10.935;1.4,11.2375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#テ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,11.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O100">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,11.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,11.335;1.4,11.6375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ト#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,11.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O101">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,12"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,11.735;1.4,12.0375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ナ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,12"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O102">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,12.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,12.135;1.4,12.4375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ニ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,12.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O103">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,12.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,12.535;1.4,12.8375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ヌ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,12.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O104">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,13.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,12.935;1.4,13.2375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ネ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,13.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O105">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,13.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,13.335;1.4,13.6375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ノ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,13.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O106">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,14"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,13.735;1.4,14.0375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ハ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,14"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O107">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,14.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,14.135;1.4,14.4375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ヒ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,14.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O108">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,14.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,14.535;1.4,14.8375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#フ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,14.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O109">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,15.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,14.935;1.4,15.2375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#へ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,15.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O110">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,15.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,15.335;1.4,15.6375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ホ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,15.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O111">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,16"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,15.735;1.4,16.0375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#マ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,16"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O112">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,16.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,16.135;1.4,16.4375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ミ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,16.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O113">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,16.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,16.535;1.4,16.8375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ム#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,16.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O114">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,17.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,16.935;1.4,17.2375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#メ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,17.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O115">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,17.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,17.335;1.4,17.6375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#モ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,17.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O116">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,18"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,17.735;1.4,18.0375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ヤ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,18"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O117">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,18.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,18.135;1.4,18.4375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ユ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,18.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O118">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.2,3.735;2.8,4.0375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.2,4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O119">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,4.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.2,4.135;2.8,4.4375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#退院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.2,4.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O120">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,4.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.2,4.535;2.8,4.8375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#在院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.2,4.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O121">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,5.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.2,4.935;3.1,5.2375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#病床数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.2,5.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O122">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,5.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.2,5.335;3.1,5.6375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#稼働率#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.2,5.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O123">
      <dia:attribute name="obj_pos">
        <dia:point val="1.6,7.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.6,6.935;3.4,7.2375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#保険内負担金#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.6,7.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O124">
      <dia:attribute name="obj_pos">
        <dia:point val="1.6,7.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.6,7.335;3.1,7.6375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#保険外金額#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.6,7.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O125">
      <dia:attribute name="obj_pos">
        <dia:point val="1.6,9.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.6,8.935;2.8,9.2375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#減免金額#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.6,9.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O126">
      <dia:attribute name="obj_pos">
        <dia:point val="1.6,9.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.6,9.335;2.8,9.6375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#助成金額#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.6,9.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O127">
      <dia:attribute name="obj_pos">
        <dia:point val="1.6,10"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.6,9.735;2.5,10.0375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#消費税#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.6,10"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O128">
      <dia:attribute name="obj_pos">
        <dia:point val="1.6,10.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.6,10.535;3.4,10.8375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#窓口請求金額#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.6,10.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O129">
      <dia:attribute name="obj_pos">
        <dia:point val="1.6,10.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.6,10.135;2.5,10.4375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#調整金#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.6,10.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O130">
      <dia:attribute name="obj_pos">
        <dia:point val="1.6,11.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.6,10.935;4,11.2375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#期間外未収金残額#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.6,11.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O131">
      <dia:attribute name="obj_pos">
        <dia:point val="1.6,11.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.6,11.335;2.5,11.6375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入金額#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.6,11.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O132">
      <dia:attribute name="obj_pos">
        <dia:point val="1.6,12"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.6,11.735;2.5,12.0375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#未収金#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.6,12"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O133">
      <dia:attribute name="obj_pos">
        <dia:point val="1.6,12.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.6,12.135;3.1,12.4375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#預り金残額#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.6,12.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O134">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,12.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.2,12.535;3.7,12.8375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#初・再診料#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.2,12.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O135">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,13.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.2,12.935;3.7,13.2375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#医学管理等#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.2,13.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O136">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,13.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.2,13.335;3.4,13.6375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#在宅医療#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.2,13.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O137">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,14"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.2,13.735;2.8,14.0375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#投薬#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.2,14"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O138">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,14.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.2,14.135;2.8,14.4375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#注射#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.2,14.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O139">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,14.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.2,14.535;2.8,14.8375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#処置#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.2,14.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O140">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,15.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.2,14.935;2.8,15.2375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#手術#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.2,15.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O141">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,15.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.2,15.335;2.8,15.6375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#麻酔#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.2,15.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O142">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,16"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.2,15.735;2.8,16.0375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#検査#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.2,16"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O143">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,16.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.2,16.135;3.4,16.4375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#画像診断#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.2,16.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O144">
      <dia:attribute name="obj_pos">
        <dia:point val="1.6,6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.6,5.735;3.1,6.0375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#保険内点数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.6,6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O145">
      <dia:attribute name="obj_pos">
        <dia:point val="1.6,6.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.6,6.535;2.5,6.8375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#平均点#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.6,6.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O146">
      <dia:attribute name="obj_pos">
        <dia:point val="28.15,3.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="28.15,2.79;28.85,3.1425"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#発行#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.15,3.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O147">
      <dia:attribute name="obj_pos">
        <dia:point val="6.05,3.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.05,2.79;7.1,3.1425"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#医師：#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.05,3.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O148">
      <dia:attribute name="obj_pos">
        <dia:point val="11.6858,1.05935"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.6858,0.61935;15.1858,1.12185"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#基  本  台  帳#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.6858,1.05935"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O149">
      <dia:attribute name="obj_pos">
        <dia:point val="1.8,14.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.65,14.335;1.95,17.6375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#診
療
区
分
別
保
険
点
数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.8,14.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O150">
      <dia:attribute name="obj_pos">
        <dia:point val="1.8,4.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.65,4.235;1.95,5.2875"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#患
者
数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.8,4.5"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O151">
      <dia:attribute name="obj_pos">
        <dia:point val="2.625,3.6125"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.625,3.3475;4.125,3.65"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#項  目  名#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.625,3.6125"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O152">
      <dia:attribute name="obj_pos">
        <dia:point val="27.9206,1.61721"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.9206,1.35221;28.2206,1.65471"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#頁#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.9206,1.61721"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O153">
      <dia:attribute name="obj_pos">
        <dia:point val="3.6,6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.6,5.8225;4.4,6.025"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#(ヌ〜ヨ)#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.25"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.6,6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O154">
      <dia:attribute name="obj_pos">
        <dia:point val="3.6,6.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.6,6.6225;4.4,6.825"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#(カ／キ)#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.25"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.6,6.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O155">
      <dia:attribute name="obj_pos">
        <dia:point val="3.6,7.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.6,7.4225;4.4,7.625"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#(あ〜さ)#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.25"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.6,7.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O156">
      <dia:attribute name="obj_pos">
        <dia:point val="1.6,8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.6,7.735;4.9,8.0375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#食事療養費・生活療養費#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.6,8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O157">
      <dia:attribute name="obj_pos">
        <dia:point val="1.6,8.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.6,8.135;4.9,8.4375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#食事負担額・生活負担額#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.6,8.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O158">
      <dia:attribute name="obj_pos">
        <dia:point val="1.6,8.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.6,8.535;2.8,8.8375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#室料差額#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.6,8.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O159">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,16.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.2,16.535;4.9,16.8375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#リハビリテーション#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.2,16.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O160">
      <dia:attribute name="obj_pos">
        <dia:point val="1.6,6.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.6,6.135;2.5,6.4375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#請求数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.6,6.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O161">
      <dia:attribute name="obj_pos">
        <dia:point val="5.20251,2.42009"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.20251,2.11009;5.55251,2.46259"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#〜#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.20251,2.42009"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O162">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,17.1995"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.2,16.9345;4.3,17.237"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#精神科専門療法#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.2,17.1995"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O163">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,17.5995"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.2,17.3345;3.7,17.637"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#放射線治療#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.2,17.5995"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O164">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,18.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.2,18.135;3.4,18.4375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院料等#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.2,18.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O165">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,18.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.2,18.535;4,18.8375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#療養担当手当#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.2,18.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O166">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,18"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.2,17.735;3.4,18.0375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#病理診断#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.2,18"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O167">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,18.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,18.535;1.4,18.8375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ヨ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,18.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
  </dia:layer>
  <dia:layer name="差し込み" visible="true" active="true">
    <dia:object type="Embed - Text" version="1" id="O168">
      <dia:attribute name="obj_pos">
        <dia:point val="28.1,3.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.25,2.79;28.1,3.1425"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#平成１１年１１月１８日#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.1,3.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TITLE.SYSYMD#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O169">
      <dia:attribute name="obj_pos">
        <dia:point val="2.45,3.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.45,2.79;5.95,3.1425"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#内科３４５６７８９０#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.45,3.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TITLE.SRYKA#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O170">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,2.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,2.09;4.95,2.4425"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#平成１４年９９年８８日#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,2.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TITLE.FROMYMD#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O171">
      <dia:attribute name="obj_pos">
        <dia:point val="7.1,3.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1,2.79;8.675,3.1425"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#日医 太郎#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.1,3.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TITLE.DRNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O172">
      <dia:attribute name="obj_pos">
        <dia:point val="15.7495,1.04522"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.7495,0.60522;19.7495,1.10772"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#(外  来  月  報)#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.7495,1.04522"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TITLE.SUBTITLE#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O173">
      <dia:attribute name="obj_pos">
        <dia:point val="11.2129,1.82277"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.2129,1.51277;15.4129,1.86527"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#対象データが存在しません#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.2129,1.82277"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TITLE.JOBMSG#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O174">
      <dia:attribute name="obj_pos">
        <dia:point val="28.8565,1.62094"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="28.3315,1.31094;28.8565,1.66344"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.8565,1.62094"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TITLE.PAGE#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O175">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,3.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,3.785;6.6875,4.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,3.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[0].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O176">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,4.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,4.185;6.6875,4.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,4.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[1].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O177">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,4.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,4.585;6.6875,4.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,4.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[2].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O178">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,5.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,4.985;6.6875,5.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,5.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[3].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O179">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,5.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,5.385;6.6875,5.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,5.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[4].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O180">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,5.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,5.785;6.6875,6.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,5.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[5].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O181">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,6.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,6.185;6.6875,6.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,6.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[6].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O182">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,6.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,6.585;6.6875,6.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,6.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[7].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O183">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,7.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,6.985;6.6875,7.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,7.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[8].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O184">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,7.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,7.385;6.6875,7.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,7.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[9].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O185">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,7.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,7.785;6.6875,8.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,7.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[10].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O186">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,8.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,8.185;6.6875,8.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,8.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[11].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O187">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,8.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,8.585;6.6875,8.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,8.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[12].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O188">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,9.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,8.985;6.6875,9.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,9.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[13].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O189">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,9.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,9.385;6.6875,9.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,9.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[14].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O190">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,9.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,9.785;6.6875,10.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,9.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[15].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O191">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,10.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,10.185;6.6875,10.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,10.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[16].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O192">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,10.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,10.585;6.6875,10.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,10.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[17].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O193">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,11.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,10.985;6.6875,11.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,11.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[18].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O194">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,11.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,11.385;6.6875,11.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,11.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[19].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O195">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,11.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,11.785;6.6875,12.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,11.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[20].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O196">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,12.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,12.185;6.6875,12.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,12.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[21].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O197">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,12.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,12.585;6.6875,12.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,12.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[22].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O198">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,13.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,12.985;6.6875,13.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,13.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[23].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O199">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,13.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,13.385;6.6875,13.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,13.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[24].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O200">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,13.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,13.785;6.6875,14.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,13.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[25].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O201">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,14.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,14.185;6.6875,14.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,14.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[26].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O202">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,14.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,14.585;6.6875,14.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,14.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[27].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O203">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,15.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,14.985;6.6875,15.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,15.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[28].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O204">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,15.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,15.385;6.6875,15.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,15.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[29].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O205">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,15.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,15.785;6.6875,16.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,15.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[30].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O206">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,16.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,16.185;6.6875,16.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,16.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[31].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O207">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,16.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,16.585;6.6875,16.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,16.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[32].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O208">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,17.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,16.985;6.6875,17.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,17.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[33].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O209">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,17.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,17.385;6.6875,17.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,17.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[34].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O210">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,18.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,18.185;6.6875,18.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,18.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[35].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O211">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,18.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,18.585;6.6875,18.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,18.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[36].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O212">
      <dia:attribute name="obj_pos">
        <dia:point val="2.8,4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.8,3.84;4.96,4.0225"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（転科、医師変更を含む）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.22500000000000001"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.8,4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TITLE.TENKI-G[0].TENKI#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O213">
      <dia:attribute name="obj_pos">
        <dia:point val="2.8,4.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.8,4.24;4.96,4.4225"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（転科、医師変更を含む）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.22500000000000001"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.8,4.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TITLE.TENKI-G[1].TENKI#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O214">
      <dia:attribute name="obj_pos">
        <dia:point val="2.8,4.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.8,4.64;4.96,4.8225"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（転科、医師変更を含む）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.22500000000000001"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.8,4.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TITLE.TENKI-G[2].TENKI#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O215">
      <dia:attribute name="obj_pos">
        <dia:point val="6.0625,3.6125"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.3125,3.3475;6.8125,3.65"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.0625,3.6125"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.TDAY-G[0].TDAY#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O216">
      <dia:attribute name="obj_pos">
        <dia:point val="7.7625,3.6125"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.0125,3.3475;8.5125,3.65"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.7625,3.6125"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.TDAY-G[1].TDAY#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O217">
      <dia:attribute name="obj_pos">
        <dia:point val="9.4625,3.6125"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.7125,3.3475;10.2125,3.65"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.4625,3.6125"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.TDAY-G[3].TDAY#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O218">
      <dia:attribute name="obj_pos">
        <dia:point val="11.1625,3.6125"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.4125,3.3475;11.9125,3.65"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.1625,3.6125"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.TDAY-G[4].TDAY#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O219">
      <dia:attribute name="obj_pos">
        <dia:point val="12.8625,3.6125"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.1125,3.3475;13.6125,3.65"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.8625,3.6125"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.TDAY-G[5].TDAY#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O220">
      <dia:attribute name="obj_pos">
        <dia:point val="19.6625,3.6125"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.9125,3.3475;20.4125,3.65"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.6625,3.6125"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.TDAY-G[6].TDAY#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O221">
      <dia:attribute name="obj_pos">
        <dia:point val="14.5625,3.6125"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.8125,3.3475;15.3125,3.65"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.5625,3.6125"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.TDAY-G[7].TDAY#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O222">
      <dia:attribute name="obj_pos">
        <dia:point val="16.2625,3.6125"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.5125,3.3475;17.0125,3.65"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.2625,3.6125"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.TDAY-G[8].TDAY#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O223">
      <dia:attribute name="obj_pos">
        <dia:point val="26.4625,3.6125"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.7125,3.3475;27.2125,3.65"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="26.4625,3.6125"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.TDAY-G[9].TDAY#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O224">
      <dia:attribute name="obj_pos">
        <dia:point val="17.9625,3.6125"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.2125,3.3475;18.7125,3.65"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.9625,3.6125"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.TDAY-G[10].TDAY#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O225">
      <dia:attribute name="obj_pos">
        <dia:point val="21.3625,3.6125"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.6125,3.3475;22.1125,3.65"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.3625,3.6125"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.TDAY-G[11].TDAY#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O226">
      <dia:attribute name="obj_pos">
        <dia:point val="23.0625,3.6125"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.3125,3.3475;23.8125,3.65"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.0625,3.6125"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.TDAY-G[12].TDAY#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O227">
      <dia:attribute name="obj_pos">
        <dia:point val="24.7625,3.6125"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.0125,3.3475;25.5125,3.65"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="24.7625,3.6125"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.TDAY-G[13].TDAY#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O228">
      <dia:attribute name="obj_pos">
        <dia:point val="28.1625,3.6125"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.4125,3.3475;28.9125,3.65"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.1625,3.6125"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.TDAY-G[14].TDAY#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O229">
      <dia:attribute name="obj_pos">
        <dia:point val="6.01404,2.42498"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.01404,2.11498;9.86404,2.46748"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#平成１４年９９年８８日#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.01404,2.42498"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TITLE.TOYMD#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O230">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6875,17.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4225,17.785;6.6875,18.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6875,17.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[0].YOSO-OCC[37].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O231">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,3.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,3.785;8.3875,4.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,3.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[0].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O232">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,4.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,4.185;8.3875,4.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,4.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[1].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O233">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,4.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,4.585;8.3875,4.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,4.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[2].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O234">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,5.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,4.985;8.3875,5.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,5.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[3].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O235">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,5.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,5.385;8.3875,5.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,5.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[4].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O236">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,5.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,5.785;8.3875,6.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,5.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[5].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O237">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,6.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,6.185;8.3875,6.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,6.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[6].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O238">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,6.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,6.585;8.3875,6.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,6.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[7].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O239">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,7.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,6.985;8.3875,7.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,7.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[8].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O240">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,7.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,7.385;8.3875,7.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,7.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[9].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O241">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,7.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,7.785;8.3875,8.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,7.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[10].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O242">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,8.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,8.185;8.3875,8.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,8.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[11].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O243">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,8.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,8.585;8.3875,8.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,8.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[12].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O244">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,9.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,8.985;8.3875,9.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,9.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[13].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O245">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,9.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,9.385;8.3875,9.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,9.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[14].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O246">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,9.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,9.785;8.3875,10.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,9.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[15].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O247">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,10.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,10.185;8.3875,10.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,10.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[16].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O248">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,10.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,10.585;8.3875,10.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,10.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[17].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O249">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,11.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,10.985;8.3875,11.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,11.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[18].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O250">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,11.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,11.385;8.3875,11.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,11.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[19].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O251">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,11.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,11.785;8.3875,12.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,11.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[20].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O252">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,12.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,12.185;8.3875,12.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,12.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[21].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O253">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,12.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,12.585;8.3875,12.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,12.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[22].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O254">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,13.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,12.985;8.3875,13.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,13.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[23].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O255">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,13.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,13.385;8.3875,13.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,13.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[24].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O256">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,13.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,13.785;8.3875,14.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,13.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[25].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O257">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,14.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,14.185;8.3875,14.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,14.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[26].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O258">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,14.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,14.585;8.3875,14.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,14.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[27].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O259">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,15.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,14.985;8.3875,15.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,15.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[28].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O260">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,15.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,15.385;8.3875,15.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,15.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[29].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O261">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,15.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,15.785;8.3875,16.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,15.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[30].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O262">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,16.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,16.185;8.3875,16.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,16.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[31].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O263">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,16.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,16.585;8.3875,16.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,16.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[32].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O264">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,17.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,16.985;8.3875,17.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,17.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[33].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O265">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,17.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,17.385;8.3875,17.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,17.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[34].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O266">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,18.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,18.185;8.3875,18.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,18.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[35].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O267">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,18.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,18.585;8.3875,18.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,18.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[36].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O268">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3875,17.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1225,17.785;8.3875,18.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3875,17.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[1].YOSO-OCC[37].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O269">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,3.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,3.785;10.0875,4.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,3.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[0].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O270">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,4.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,4.185;10.0875,4.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,4.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[1].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O271">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,4.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,4.585;10.0875,4.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,4.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[2].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O272">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,5.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,4.985;10.0875,5.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,5.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[3].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O273">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,5.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,5.385;10.0875,5.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,5.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[4].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O274">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,5.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,5.785;10.0875,6.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,5.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[5].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O275">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,6.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,6.185;10.0875,6.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,6.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[6].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O276">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,6.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,6.585;10.0875,6.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,6.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[7].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O277">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,7.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,6.985;10.0875,7.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,7.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[8].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O278">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,7.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,7.385;10.0875,7.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,7.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[9].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O279">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,7.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,7.785;10.0875,8.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,7.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[10].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O280">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,8.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,8.185;10.0875,8.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,8.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[11].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O281">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,8.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,8.585;10.0875,8.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,8.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[12].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O282">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,9.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,8.985;10.0875,9.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,9.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[13].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O283">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,9.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,9.385;10.0875,9.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,9.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[14].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O284">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,9.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,9.785;10.0875,10.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,9.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[15].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O285">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,10.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,10.185;10.0875,10.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,10.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[16].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O286">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,10.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,10.585;10.0875,10.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,10.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[17].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O287">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,11.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,10.985;10.0875,11.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,11.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[18].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O288">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,11.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,11.385;10.0875,11.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,11.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[19].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O289">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,11.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,11.785;10.0875,12.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,11.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[20].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O290">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,12.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,12.185;10.0875,12.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,12.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[21].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O291">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,12.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,12.585;10.0875,12.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,12.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[22].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O292">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,13.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,12.985;10.0875,13.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,13.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[23].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O293">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,13.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,13.385;10.0875,13.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,13.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[24].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O294">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,13.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,13.785;10.0875,14.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,13.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[25].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O295">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,14.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,14.185;10.0875,14.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,14.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[26].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O296">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,14.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,14.585;10.0875,14.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,14.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[27].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O297">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,15.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,14.985;10.0875,15.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,15.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[28].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O298">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,15.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,15.385;10.0875,15.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,15.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[29].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O299">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,15.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,15.785;10.0875,16.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,15.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[30].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O300">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,16.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,16.185;10.0875,16.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,16.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[31].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O301">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,16.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,16.585;10.0875,16.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,16.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[32].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O302">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,17.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,16.985;10.0875,17.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,17.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[33].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O303">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,17.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,17.385;10.0875,17.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,17.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[34].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O304">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,18.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,18.185;10.0875,18.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,18.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[35].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O305">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,18.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,18.585;10.0875,18.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,18.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[36].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O306">
      <dia:attribute name="obj_pos">
        <dia:point val="10.0875,17.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8225,17.785;10.0875,18.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.0875,17.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[3].YOSO-OCC[37].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O307">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,3.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,3.785;11.7875,4.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,3.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[0].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O308">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,4.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,4.185;11.7875,4.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,4.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[1].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O309">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,4.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,4.585;11.7875,4.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,4.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[2].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O310">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,5.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,4.985;11.7875,5.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,5.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[3].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O311">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,5.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,5.385;11.7875,5.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,5.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[4].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O312">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,5.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,5.785;11.7875,6.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,5.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[5].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O313">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,6.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,6.185;11.7875,6.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,6.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[6].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O314">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,6.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,6.585;11.7875,6.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,6.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[7].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O315">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,7.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,6.985;11.7875,7.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,7.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[8].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O316">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,7.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,7.385;11.7875,7.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,7.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[9].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O317">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,7.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,7.785;11.7875,8.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,7.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[10].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O318">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,8.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,8.185;11.7875,8.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,8.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[11].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O319">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,8.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,8.585;11.7875,8.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,8.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[12].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O320">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,9.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,8.985;11.7875,9.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,9.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[13].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O321">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,9.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,9.385;11.7875,9.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,9.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[14].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O322">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,9.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,9.785;11.7875,10.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,9.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[15].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O323">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,10.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,10.185;11.7875,10.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,10.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[16].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O324">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,10.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,10.585;11.7875,10.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,10.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[17].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O325">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,11.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,10.985;11.7875,11.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,11.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[18].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O326">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,11.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,11.385;11.7875,11.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,11.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[19].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O327">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,11.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,11.785;11.7875,12.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,11.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[20].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O328">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,12.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,12.185;11.7875,12.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,12.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[21].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O329">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,12.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,12.585;11.7875,12.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,12.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[22].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O330">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,13.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,12.985;11.7875,13.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,13.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[23].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O331">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,13.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,13.385;11.7875,13.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,13.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[24].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O332">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,13.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,13.785;11.7875,14.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,13.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[25].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O333">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,14.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,14.185;11.7875,14.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,14.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[26].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O334">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,14.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,14.585;11.7875,14.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,14.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[27].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O335">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,15.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,14.985;11.7875,15.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,15.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[28].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O336">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,15.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,15.385;11.7875,15.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,15.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[29].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O337">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,15.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,15.785;11.7875,16.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,15.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[30].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O338">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,16.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,16.185;11.7875,16.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,16.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[31].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O339">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,16.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,16.585;11.7875,16.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,16.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[32].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O340">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,17.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,16.985;11.7875,17.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,17.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[33].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O341">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,17.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,17.385;11.7875,17.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,17.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[34].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O342">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,18.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,18.185;11.7875,18.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,18.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[35].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O343">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,18.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,18.585;11.7875,18.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,18.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[36].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O344">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7875,17.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5225,17.785;11.7875,18.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7875,17.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[4].YOSO-OCC[37].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O345">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,3.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,3.785;13.4875,4.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,3.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[0].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O346">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,4.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,4.185;13.4875,4.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,4.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[1].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O347">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,4.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,4.585;13.4875,4.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,4.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[2].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O348">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,5.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,4.985;13.4875,5.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,5.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[3].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O349">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,5.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,5.385;13.4875,5.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,5.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[4].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O350">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,5.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,5.785;13.4875,6.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,5.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[5].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O351">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,6.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,6.185;13.4875,6.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,6.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[6].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O352">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,6.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,6.585;13.4875,6.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,6.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[7].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O353">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,7.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,6.985;13.4875,7.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,7.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[8].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O354">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,7.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,7.385;13.4875,7.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,7.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[9].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O355">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,7.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,7.785;13.4875,8.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,7.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[10].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O356">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,8.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,8.185;13.4875,8.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,8.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[11].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O357">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,8.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,8.585;13.4875,8.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,8.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[12].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O358">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,9.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,8.985;13.4875,9.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,9.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[13].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O359">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,9.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,9.385;13.4875,9.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,9.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[14].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O360">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,9.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,9.785;13.4875,10.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,9.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[15].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O361">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,10.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,10.185;13.4875,10.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,10.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[16].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O362">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,10.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,10.585;13.4875,10.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,10.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[17].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O363">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,11.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,10.985;13.4875,11.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,11.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[18].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O364">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,11.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,11.385;13.4875,11.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,11.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[19].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O365">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,11.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,11.785;13.4875,12.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,11.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[20].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O366">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,12.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,12.185;13.4875,12.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,12.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[21].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O367">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,12.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,12.585;13.4875,12.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,12.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[22].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O368">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,13.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,12.985;13.4875,13.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,13.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[23].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O369">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,13.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,13.385;13.4875,13.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,13.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[24].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O370">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,13.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,13.785;13.4875,14.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,13.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[25].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O371">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,14.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,14.185;13.4875,14.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,14.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[26].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O372">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,14.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,14.585;13.4875,14.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,14.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[27].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O373">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,15.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,14.985;13.4875,15.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,15.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[28].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O374">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,15.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,15.385;13.4875,15.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,15.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[29].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O375">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,15.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,15.785;13.4875,16.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,15.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[30].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O376">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,16.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,16.185;13.4875,16.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,16.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[31].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O377">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,16.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,16.585;13.4875,16.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,16.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[32].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O378">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,17.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,16.985;13.4875,17.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,17.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[33].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O379">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,17.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,17.385;13.4875,17.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,17.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[34].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O380">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,18.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,18.185;13.4875,18.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,18.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[35].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O381">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,18.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,18.585;13.4875,18.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,18.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[36].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O382">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4875,17.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2225,17.785;13.4875,18.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4875,17.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[5].YOSO-OCC[37].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O383">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,3.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,3.785;20.2875,4.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,3.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[0].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O384">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,4.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,4.185;20.2875,4.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,4.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[1].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O385">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,4.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,4.585;20.2875,4.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,4.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[2].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O386">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,5.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,4.985;20.2875,5.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,5.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[3].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O387">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,5.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,5.385;20.2875,5.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,5.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[4].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O388">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,5.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,5.785;20.2875,6.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,5.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[5].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O389">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,6.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,6.185;20.2875,6.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,6.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[6].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O390">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,6.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,6.585;20.2875,6.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,6.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[7].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O391">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,7.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,6.985;20.2875,7.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,7.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[8].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O392">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,7.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,7.385;20.2875,7.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,7.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[9].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O393">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,7.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,7.785;20.2875,8.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,7.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[10].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O394">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,8.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,8.185;20.2875,8.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,8.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[11].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O395">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,8.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,8.585;20.2875,8.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,8.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[12].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O396">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,9.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,8.985;20.2875,9.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,9.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[13].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O397">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,9.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,9.385;20.2875,9.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,9.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[14].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O398">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,9.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,9.785;20.2875,10.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,9.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[15].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O399">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,10.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,10.185;20.2875,10.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,10.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[16].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O400">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,10.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,10.585;20.2875,10.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,10.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[17].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O401">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,11.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,10.985;20.2875,11.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,11.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[18].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O402">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,11.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,11.385;20.2875,11.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,11.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[19].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O403">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,11.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,11.785;20.2875,12.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,11.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[20].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O404">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,12.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,12.185;20.2875,12.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,12.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[21].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O405">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,12.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,12.585;20.2875,12.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,12.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[22].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O406">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,13.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,12.985;20.2875,13.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,13.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[23].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O407">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,13.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,13.385;20.2875,13.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,13.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[24].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O408">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,13.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,13.785;20.2875,14.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,13.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[25].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O409">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,14.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,14.185;20.2875,14.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,14.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[26].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O410">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,14.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,14.585;20.2875,14.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,14.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[27].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O411">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,15.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,14.985;20.2875,15.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,15.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[28].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O412">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,15.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,15.385;20.2875,15.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,15.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[29].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O413">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,15.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,15.785;20.2875,16.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,15.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[30].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O414">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,16.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,16.185;20.2875,16.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,16.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[31].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O415">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,16.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,16.585;20.2875,16.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,16.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[32].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O416">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,17.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,16.985;20.2875,17.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,17.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[33].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O417">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,17.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,17.385;20.2875,17.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,17.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[34].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O418">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,18.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,18.185;20.2875,18.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,18.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[35].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O419">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,18.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,18.585;20.2875,18.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,18.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[36].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O420">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2875,17.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0225,17.785;20.2875,18.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2875,17.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[6].YOSO-OCC[37].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O421">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,3.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,3.785;15.1875,4.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,3.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[0].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O422">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,4.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,4.185;15.1875,4.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,4.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[1].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O423">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,4.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,4.585;15.1875,4.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,4.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[2].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O424">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,5.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,4.985;15.1875,5.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,5.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[3].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O425">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,5.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,5.385;15.1875,5.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,5.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[4].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O426">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,5.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,5.785;15.1875,6.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,5.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[5].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O427">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,6.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,6.185;15.1875,6.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,6.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[6].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O428">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,6.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,6.585;15.1875,6.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,6.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[7].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O429">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,7.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,6.985;15.1875,7.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,7.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[8].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O430">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,7.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,7.385;15.1875,7.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,7.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[9].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O431">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,7.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,7.785;15.1875,8.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,7.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[10].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O432">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,8.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,8.185;15.1875,8.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,8.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[11].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O433">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,8.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,8.585;15.1875,8.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,8.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[12].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O434">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,9.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,8.985;15.1875,9.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,9.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[13].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O435">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,9.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,9.385;15.1875,9.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,9.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[14].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O436">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,9.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,9.785;15.1875,10.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,9.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[15].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O437">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,10.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,10.185;15.1875,10.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,10.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[16].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O438">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,10.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,10.585;15.1875,10.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,10.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[17].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O439">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,11.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,10.985;15.1875,11.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,11.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[18].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O440">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,11.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,11.385;15.1875,11.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,11.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[19].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O441">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,11.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,11.785;15.1875,12.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,11.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[20].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O442">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,12.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,12.185;15.1875,12.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,12.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[21].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O443">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,12.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,12.585;15.1875,12.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,12.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[22].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O444">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,13.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,12.985;15.1875,13.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,13.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[23].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O445">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,13.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,13.385;15.1875,13.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,13.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[24].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O446">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,13.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,13.785;15.1875,14.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,13.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[25].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O447">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,14.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,14.185;15.1875,14.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,14.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[26].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O448">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,14.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,14.585;15.1875,14.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,14.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[27].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O449">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,15.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,14.985;15.1875,15.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,15.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[28].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O450">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,15.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,15.385;15.1875,15.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,15.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[29].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O451">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,15.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,15.785;15.1875,16.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,15.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[30].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O452">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,16.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,16.185;15.1875,16.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,16.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[31].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O453">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,16.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,16.585;15.1875,16.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,16.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[32].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O454">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,17.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,16.985;15.1875,17.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,17.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[33].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O455">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,17.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,17.385;15.1875,17.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,17.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[34].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O456">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,18.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,18.185;15.1875,18.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,18.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[35].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O457">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,18.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,18.585;15.1875,18.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,18.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[36].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O458">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1875,17.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9225,17.785;15.1875,18.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1875,17.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[7].YOSO-OCC[37].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O459">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,3.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,3.785;16.8875,4.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,3.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[0].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O460">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,4.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,4.185;16.8875,4.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,4.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[1].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O461">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,4.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,4.585;16.8875,4.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,4.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[2].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O462">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,5.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,4.985;16.8875,5.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,5.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[3].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O463">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,5.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,5.385;16.8875,5.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,5.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[4].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O464">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,5.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,5.785;16.8875,6.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,5.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[5].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O465">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,6.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,6.185;16.8875,6.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,6.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[6].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O466">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,6.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,6.585;16.8875,6.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,6.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[7].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O467">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,7.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,6.985;16.8875,7.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,7.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[8].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O468">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,7.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,7.385;16.8875,7.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,7.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[9].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O469">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,7.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,7.785;16.8875,8.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,7.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[10].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O470">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,8.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,8.185;16.8875,8.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,8.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[11].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O471">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,8.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,8.585;16.8875,8.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,8.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[12].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O472">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,9.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,8.985;16.8875,9.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,9.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[13].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O473">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,9.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,9.385;16.8875,9.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,9.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[14].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O474">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,9.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,9.785;16.8875,10.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,9.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[15].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O475">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,10.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,10.185;16.8875,10.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,10.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[16].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O476">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,10.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,10.585;16.8875,10.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,10.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[17].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O477">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,11.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,10.985;16.8875,11.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,11.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[18].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O478">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,11.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,11.385;16.8875,11.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,11.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[19].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O479">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,11.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,11.785;16.8875,12.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,11.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[20].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O480">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,12.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,12.185;16.8875,12.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,12.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[21].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O481">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,12.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,12.585;16.8875,12.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,12.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[22].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O482">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,13.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,12.985;16.8875,13.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,13.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[23].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O483">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,13.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,13.385;16.8875,13.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,13.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[24].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O484">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,13.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,13.785;16.8875,14.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,13.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[25].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O485">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,14.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,14.185;16.8875,14.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,14.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[26].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O486">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,14.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,14.585;16.8875,14.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,14.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[27].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O487">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,15.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,14.985;16.8875,15.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,15.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[28].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O488">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,15.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,15.385;16.8875,15.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,15.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[29].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O489">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,15.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,15.785;16.8875,16.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,15.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[30].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O490">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,16.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,16.185;16.8875,16.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,16.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[31].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O491">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,16.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,16.585;16.8875,16.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,16.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[32].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O492">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,17.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,16.985;16.8875,17.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,17.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[33].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O493">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,17.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,17.385;16.8875,17.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,17.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[34].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O494">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,18.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,18.185;16.8875,18.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,18.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[35].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O495">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,18.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,18.585;16.8875,18.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,18.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[36].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O496">
      <dia:attribute name="obj_pos">
        <dia:point val="16.8875,17.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.6225,17.785;16.8875,18.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8875,17.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[8].YOSO-OCC[37].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O497">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,3.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,3.785;27.0875,4.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,3.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[0].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O498">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,4.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,4.185;27.0875,4.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,4.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[1].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O499">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,4.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,4.585;27.0875,4.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,4.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[2].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O500">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,5.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,4.985;27.0875,5.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,5.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[3].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O501">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,5.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,5.385;27.0875,5.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,5.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[4].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O502">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,5.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,5.785;27.0875,6.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,5.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[5].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O503">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,6.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,6.185;27.0875,6.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,6.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[6].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O504">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,6.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,6.585;27.0875,6.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,6.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[7].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O505">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,7.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,6.985;27.0875,7.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,7.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[8].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O506">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,7.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,7.385;27.0875,7.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,7.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[9].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O507">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,7.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,7.785;27.0875,8.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,7.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[10].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O508">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,8.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,8.185;27.0875,8.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,8.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[11].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O509">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,8.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,8.585;27.0875,8.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,8.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[12].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O510">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,9.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,8.985;27.0875,9.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,9.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[13].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O511">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,9.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,9.385;27.0875,9.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,9.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[14].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O512">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,9.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,9.785;27.0875,10.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,9.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[15].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O513">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,10.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,10.185;27.0875,10.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,10.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[16].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O514">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,10.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,10.585;27.0875,10.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,10.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[17].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O515">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,11.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,10.985;27.0875,11.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,11.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[18].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O516">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,11.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,11.385;27.0875,11.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,11.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[19].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O517">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,11.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,11.785;27.0875,12.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,11.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[20].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O518">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,12.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,12.185;27.0875,12.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,12.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[21].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O519">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,12.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,12.585;27.0875,12.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,12.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[22].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O520">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,13.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,12.985;27.0875,13.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,13.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[23].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O521">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,13.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,13.385;27.0875,13.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,13.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[24].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O522">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,13.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,13.785;27.0875,14.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,13.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[25].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O523">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,14.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,14.185;27.0875,14.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,14.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[26].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O524">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,14.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,14.585;27.0875,14.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,14.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[27].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O525">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,15.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,14.985;27.0875,15.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,15.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[28].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O526">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,15.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,15.385;27.0875,15.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,15.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[29].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O527">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,15.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,15.785;27.0875,16.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,15.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[30].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O528">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,16.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,16.185;27.0875,16.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,16.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[31].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O529">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,16.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,16.585;27.0875,16.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,16.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[32].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O530">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,17.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,16.985;27.0875,17.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,17.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[33].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O531">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,17.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,17.385;27.0875,17.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,17.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[34].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O532">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,18.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,18.185;27.0875,18.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,18.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[35].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O533">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,18.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,18.585;27.0875,18.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,18.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[36].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O534">
      <dia:attribute name="obj_pos">
        <dia:point val="27.0875,17.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="25.8225,17.785;27.0875,18.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="27.0875,17.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[9].YOSO-OCC[37].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O535">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,3.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,3.785;18.5875,4.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,3.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[0].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O536">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,4.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,4.185;18.5875,4.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,4.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[1].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O537">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,4.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,4.585;18.5875,4.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,4.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[2].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O538">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,5.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,4.985;18.5875,5.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,5.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[3].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O539">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,5.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,5.385;18.5875,5.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,5.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[4].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O540">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,5.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,5.785;18.5875,6.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,5.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[5].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O541">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,6.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,6.185;18.5875,6.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,6.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[6].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O542">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,6.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,6.585;18.5875,6.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,6.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[7].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O543">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,7.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,6.985;18.5875,7.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,7.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[8].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O544">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,7.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,7.385;18.5875,7.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,7.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[9].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O545">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,7.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,7.785;18.5875,8.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,7.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[10].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O546">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,8.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,8.185;18.5875,8.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,8.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[11].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O547">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,8.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,8.585;18.5875,8.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,8.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[12].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O548">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,9.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,8.985;18.5875,9.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,9.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[13].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O549">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,9.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,9.385;18.5875,9.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,9.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[14].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O550">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,9.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,9.785;18.5875,10.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,9.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[15].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O551">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,10.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,10.185;18.5875,10.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,10.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[16].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O552">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,10.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,10.585;18.5875,10.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,10.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[17].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O553">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,11.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,10.985;18.5875,11.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,11.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[18].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O554">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,11.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,11.385;18.5875,11.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,11.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[19].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O555">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,11.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,11.785;18.5875,12.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,11.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[20].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O556">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,12.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,12.185;18.5875,12.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,12.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[21].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O557">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,12.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,12.585;18.5875,12.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,12.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[22].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O558">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,13.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,12.985;18.5875,13.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,13.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[23].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O559">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,13.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,13.385;18.5875,13.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,13.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[24].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O560">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,13.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,13.785;18.5875,14.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,13.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[25].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O561">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,14.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,14.185;18.5875,14.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,14.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[26].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O562">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,14.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,14.585;18.5875,14.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,14.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[27].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O563">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,15.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,14.985;18.5875,15.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,15.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[28].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O564">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,15.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,15.385;18.5875,15.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,15.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[29].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O565">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,15.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,15.785;18.5875,16.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,15.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[30].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O566">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,16.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,16.185;18.5875,16.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,16.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[31].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O567">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,16.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,16.585;18.5875,16.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,16.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[32].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O568">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,17.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,16.985;18.5875,17.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,17.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[33].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O569">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,17.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,17.385;18.5875,17.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,17.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[34].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O570">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,18.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,18.185;18.5875,18.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,18.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[35].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O571">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,18.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,18.585;18.5875,18.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,18.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[36].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O572">
      <dia:attribute name="obj_pos">
        <dia:point val="18.5875,17.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3225,17.785;18.5875,18.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.5875,17.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[10].YOSO-OCC[37].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O573">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,3.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,3.785;21.9875,4.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,3.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[0].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O574">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,4.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,4.185;21.9875,4.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,4.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[1].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O575">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,4.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,4.585;21.9875,4.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,4.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[2].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O576">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,5.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,4.985;21.9875,5.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,5.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[3].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O577">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,5.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,5.385;21.9875,5.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,5.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[4].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O578">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,5.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,5.785;21.9875,6.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,5.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[5].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O579">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,6.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,6.185;21.9875,6.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,6.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[6].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O580">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,6.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,6.585;21.9875,6.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,6.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[7].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O581">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,7.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,6.985;21.9875,7.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,7.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[8].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O582">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,7.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,7.385;21.9875,7.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,7.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[9].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O583">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,7.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,7.785;21.9875,8.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,7.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[10].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O584">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,8.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,8.185;21.9875,8.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,8.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[11].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O585">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,8.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,8.585;21.9875,8.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,8.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[12].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O586">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,9.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,8.985;21.9875,9.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,9.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[13].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O587">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,9.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,9.385;21.9875,9.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,9.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[14].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O588">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,9.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,9.785;21.9875,10.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,9.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[15].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O589">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,10.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,10.185;21.9875,10.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,10.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[16].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O590">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,10.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,10.585;21.9875,10.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,10.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[17].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O591">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,11.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,10.985;21.9875,11.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,11.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[18].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O592">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,11.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,11.385;21.9875,11.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,11.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[19].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O593">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,11.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,11.785;21.9875,12.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,11.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[20].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O594">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,12.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,12.185;21.9875,12.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,12.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[21].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O595">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,12.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,12.585;21.9875,12.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,12.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[22].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O596">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,13.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,12.985;21.9875,13.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,13.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[23].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O597">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,13.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,13.385;21.9875,13.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,13.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[24].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O598">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,13.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,13.785;21.9875,14.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,13.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[25].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O599">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,14.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,14.185;21.9875,14.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,14.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[26].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O600">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,14.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,14.585;21.9875,14.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,14.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[27].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O601">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,15.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,14.985;21.9875,15.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,15.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[28].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O602">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,15.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,15.385;21.9875,15.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,15.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[29].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O603">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,15.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,15.785;21.9875,16.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,15.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[30].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O604">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,16.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,16.185;21.9875,16.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,16.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[31].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O605">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,16.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,16.585;21.9875,16.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,16.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[32].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O606">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,17.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,16.985;21.9875,17.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,17.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[33].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O607">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,17.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,17.385;21.9875,17.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,17.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[34].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O608">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,18.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,18.185;21.9875,18.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,18.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[35].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O609">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,18.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,18.585;21.9875,18.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,18.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[36].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O610">
      <dia:attribute name="obj_pos">
        <dia:point val="21.9875,17.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="20.7225,17.785;21.9875,18.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="21.9875,17.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[11].YOSO-OCC[37].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O611">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,3.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,3.785;23.6875,4.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,3.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[0].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O612">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,4.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,4.185;23.6875,4.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,4.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[1].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O613">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,4.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,4.585;23.6875,4.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,4.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[2].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O614">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,5.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,4.985;23.6875,5.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,5.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[3].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O615">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,5.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,5.385;23.6875,5.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,5.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[4].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O616">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,5.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,5.785;23.6875,6.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,5.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[5].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O617">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,6.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,6.185;23.6875,6.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,6.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[6].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O618">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,6.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,6.585;23.6875,6.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,6.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[7].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O619">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,7.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,6.985;23.6875,7.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,7.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[8].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O620">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,7.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,7.385;23.6875,7.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,7.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[9].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O621">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,7.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,7.785;23.6875,8.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,7.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[10].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O622">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,8.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,8.185;23.6875,8.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,8.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[11].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O623">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,8.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,8.585;23.6875,8.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,8.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[12].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O624">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,9.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,8.985;23.6875,9.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,9.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[13].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O625">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,9.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,9.385;23.6875,9.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,9.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[14].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O626">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,9.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,9.785;23.6875,10.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,9.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[15].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O627">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,10.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,10.185;23.6875,10.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,10.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[16].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O628">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,10.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,10.585;23.6875,10.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,10.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[17].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O629">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,11.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,10.985;23.6875,11.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,11.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[18].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O630">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,11.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,11.385;23.6875,11.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,11.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[19].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O631">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,11.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,11.785;23.6875,12.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,11.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[20].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O632">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,12.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,12.185;23.6875,12.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,12.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[21].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O633">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,12.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,12.585;23.6875,12.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,12.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[22].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O634">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,13.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,12.985;23.6875,13.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,13.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[23].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O635">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,13.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,13.385;23.6875,13.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,13.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[24].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O636">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,13.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,13.785;23.6875,14.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,13.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[25].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O637">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,14.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,14.185;23.6875,14.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,14.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[26].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O638">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,14.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,14.585;23.6875,14.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,14.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[27].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O639">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,15.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,14.985;23.6875,15.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,15.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[28].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O640">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,15.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,15.385;23.6875,15.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,15.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[29].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O641">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,15.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,15.785;23.6875,16.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,15.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[30].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O642">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,16.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,16.185;23.6875,16.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,16.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[31].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O643">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,16.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,16.585;23.6875,16.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,16.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[32].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O644">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,17.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,16.985;23.6875,17.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,17.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[33].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O645">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,17.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,17.385;23.6875,17.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,17.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[34].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O646">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,18.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,18.185;23.6875,18.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,18.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[35].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O647">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,18.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,18.585;23.6875,18.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,18.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[36].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O648">
      <dia:attribute name="obj_pos">
        <dia:point val="23.6875,17.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="22.4225,17.785;23.6875,18.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="23.6875,17.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[12].YOSO-OCC[37].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O649">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,3.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,3.785;25.3875,4.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,3.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[0].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O650">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,4.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,4.185;25.3875,4.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,4.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[1].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O651">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,4.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,4.585;25.3875,4.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,4.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[2].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O652">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,5.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,4.985;25.3875,5.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,5.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[3].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O653">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,5.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,5.385;25.3875,5.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,5.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[4].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O654">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,5.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,5.785;25.3875,6.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,5.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[5].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O655">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,6.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,6.185;25.3875,6.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,6.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[6].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O656">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,6.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,6.585;25.3875,6.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,6.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[7].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O657">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,7.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,6.985;25.3875,7.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,7.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[8].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O658">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,7.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,7.385;25.3875,7.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,7.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[9].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O659">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,7.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,7.785;25.3875,8.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,7.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[10].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O660">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,8.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,8.185;25.3875,8.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,8.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[11].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O661">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,8.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,8.585;25.3875,8.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,8.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[12].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O662">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,9.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,8.985;25.3875,9.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,9.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[13].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O663">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,9.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,9.385;25.3875,9.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,9.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[14].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O664">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,9.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,9.785;25.3875,10.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,9.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[15].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O665">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,10.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,10.185;25.3875,10.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,10.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[16].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O666">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,10.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,10.585;25.3875,10.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,10.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[17].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O667">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,11.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,10.985;25.3875,11.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,11.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[18].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O668">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,11.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,11.385;25.3875,11.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,11.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[19].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O669">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,11.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,11.785;25.3875,12.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,11.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[20].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O670">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,12.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,12.185;25.3875,12.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,12.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[21].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O671">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,12.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,12.585;25.3875,12.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,12.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[22].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O672">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,13.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,12.985;25.3875,13.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,13.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[23].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O673">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,13.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,13.385;25.3875,13.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,13.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[24].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O674">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,13.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,13.785;25.3875,14.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,13.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[25].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O675">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,14.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,14.185;25.3875,14.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,14.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[26].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O676">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,14.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,14.585;25.3875,14.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,14.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[27].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O677">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,15.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,14.985;25.3875,15.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,15.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[28].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O678">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,15.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,15.385;25.3875,15.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,15.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[29].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O679">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,15.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,15.785;25.3875,16.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,15.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[30].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O680">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,16.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,16.185;25.3875,16.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,16.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[31].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O681">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,16.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,16.585;25.3875,16.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,16.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[32].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O682">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,17.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,16.985;25.3875,17.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,17.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[33].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O683">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,17.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,17.385;25.3875,17.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,17.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[34].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O684">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,18.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,18.185;25.3875,18.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,18.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[35].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O685">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,18.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,18.585;25.3875,18.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,18.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[36].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O686">
      <dia:attribute name="obj_pos">
        <dia:point val="25.3875,17.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="24.1225,17.785;25.3875,18.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="25.3875,17.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[13].YOSO-OCC[37].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O687">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,3.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,3.785;28.7875,4.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,3.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[0].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O688">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,4.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,4.185;28.7875,4.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,4.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[1].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O689">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,4.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,4.585;28.7875,4.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,4.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[2].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O690">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,5.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,4.985;28.7875,5.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,5.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[3].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O691">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,5.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,5.385;28.7875,5.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,5.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[4].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O692">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,5.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,5.785;28.7875,6.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,5.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[5].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O693">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,6.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,6.185;28.7875,6.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,6.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[6].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O694">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,6.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,6.585;28.7875,6.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,6.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[7].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O695">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,7.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,6.985;28.7875,7.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,7.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[8].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O696">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,7.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,7.385;28.7875,7.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,7.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[9].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O697">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,7.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,7.785;28.7875,8.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,7.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[10].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O698">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,8.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,8.185;28.7875,8.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,8.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[11].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O699">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,8.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,8.585;28.7875,8.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,8.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[12].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O700">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,9.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,8.985;28.7875,9.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,9.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[13].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O701">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,9.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,9.385;28.7875,9.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,9.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[14].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O702">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,9.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,9.785;28.7875,10.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,9.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[15].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O703">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,10.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,10.185;28.7875,10.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,10.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[16].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O704">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,10.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,10.585;28.7875,10.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,10.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[17].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O705">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,11.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,10.985;28.7875,11.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,11.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[18].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O706">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,11.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,11.385;28.7875,11.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,11.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[19].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O707">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,11.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,11.785;28.7875,12.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,11.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[20].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O708">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,12.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,12.185;28.7875,12.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,12.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[21].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O709">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,12.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,12.585;28.7875,12.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,12.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[22].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O710">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,13.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,12.985;28.7875,13.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,13.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[23].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O711">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,13.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,13.385;28.7875,13.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,13.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[24].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O712">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,13.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,13.785;28.7875,14.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,13.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[25].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O713">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,14.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,14.185;28.7875,14.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,14.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[26].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O714">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,14.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,14.585;28.7875,14.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,14.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[27].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O715">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,15.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,14.985;28.7875,15.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,15.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[28].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O716">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,15.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,15.385;28.7875,15.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,15.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[29].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O717">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,15.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,15.785;28.7875,16.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,15.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[30].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O718">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,16.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,16.185;28.7875,16.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,16.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[31].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O719">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,16.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,16.585;28.7875,16.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,16.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[32].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O720">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,17.1875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,16.985;28.7875,17.2175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,17.1875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[33].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O721">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,17.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,17.385;28.7875,17.6175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,17.5875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[34].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O722">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,18.3875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,18.185;28.7875,18.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,18.3875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[35].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O723">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,18.7875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,18.585;28.7875,18.8175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,18.7875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[36].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O724">
      <dia:attribute name="obj_pos">
        <dia:point val="28.7875,17.9875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="27.5225,17.785;28.7875,18.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.28749999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="28.7875,17.9875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.YOSO-G[14].YOSO-OCC[37].YOSO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="1"/>
      </dia:attribute>
    </dia:object>
  </dia:layer>
</dia:diagram>
