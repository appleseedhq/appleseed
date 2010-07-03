<?xml version="1.0" encoding="utf-8"?>
<!--
   Copyright (c) 2002 Douglas Gregor <doug.gregor -at- gmail.com>

   Distributed under the Boost Software License, Version 1.0.
   (See accompanying file LICENSE_1_0.txt or copy at
   http://www.boost.org/LICENSE_1_0.txt)
  -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:rev="http://www.cs.rpi.edu/~gregod/boost/tools/doc/revision"
                version="1.0">

  <!-- Import the HTML chunking stylesheet -->
  <xsl:import
    href="http://docbook.sourceforge.net/release/xsl/current/html/chunk.xsl"/>
  <xsl:import
    href="http://docbook.sourceforge.net/release/xsl/current/html/math.xsl"/>

  <xsl:import href="chunk-common.xsl"/>
  <xsl:import href="docbook-layout.xsl"/>
  <xsl:import href="navbar.xsl"/>
  <xsl:import href="admon.xsl"/>
  <xsl:import href="xref.xsl"/>
  <xsl:import href="relative-href.xsl"/>
  <xsl:import href="callout.xsl"/>

  <xsl:param name="admon.style"/>
  <xsl:param name="admon.graphics">1</xsl:param>
  <xsl:param name="boostbook.verbose" select="0"/>
  <xsl:param name="html.stylesheet" select="'boostbook.css'"/>
  <xsl:param name="navig.graphics" select="1"/>
  <xsl:param name="navig.graphics.extension" select="'.png'"/>
  <xsl:param name="chapter.autolabel" select="1"/>
  <xsl:param name="use.id.as.filename" select="1"/>
  <xsl:param name="refentry.generate.name" select="0"/>
  <xsl:param name="refentry.generate.title" select="1"/>
  <xsl:param name="make.year.ranges" select="1"/>
  <xsl:param name="generate.manifest" select="1"/>
  <xsl:param name="generate.section.toc.level" select="3"/>
  <xsl:param name="doc.standalone">false</xsl:param>
  <xsl:param name="chunker.output.indent">yes</xsl:param>
  <xsl:param name="chunk.quietly" select="not(number($boostbook.verbose))"/>
  <xsl:param name="toc.max.depth">2</xsl:param>
  <xsl:param name="callout.graphics.number.limit">15</xsl:param>
  <xsl:param name = "admon.graphics.path"
            select = "concat($boost.root, '/doc/html/images/')"/>
  <xsl:param name = "navig.graphics.path"
            select = "concat($boost.root, '/doc/html/images/')"/>
  <xsl:param name = "callout.graphics.path"
            select = "concat($boost.root, '/doc/src/images/callouts/')"/>


  <xsl:param name="admon.style">
    <!-- Remove the style. Let the CSS do the styling -->
</xsl:param>

<!-- Always have graphics -->
<xsl:param name="admon.graphics" select="1"/>

  <xsl:param name="generate.toc">
appendix  toc,title
article/appendix  nop
article   toc,title
book      toc,title
chapter   toc,title
part      toc,title
preface   toc,title
qandadiv  toc
qandaset  toc
reference toc,title
sect1     toc
sect2     toc
sect3     toc
sect4     toc
sect5     toc
section   toc
set       toc,title
  </xsl:param>


  <xsl:template name="format.cvs.revision">
    <xsl:param name="text"/>

    <!-- Remove the "$Date: " -->
    <xsl:variable name="text.noprefix"
      select="substring-after($text, '$Date: ')"/>

    <!-- Grab the year -->
    <xsl:variable name="year" select="substring-before($text.noprefix, '/')"/>
    <xsl:variable name="text.noyear"
      select="substring-after($text.noprefix, '/')"/>

    <!-- Grab the month -->
    <xsl:variable name="month" select="substring-before($text.noyear, '/')"/>
    <xsl:variable name="text.nomonth"
      select="substring-after($text.noyear, '/')"/>

    <!-- Grab the year -->
    <xsl:variable name="day" select="substring-before($text.nomonth, ' ')"/>
    <xsl:variable name="text.noday"
      select="substring-after($text.nomonth, ' ')"/>

    <!-- Get the time -->
    <xsl:variable name="time" select="substring-before($text.noday, ' ')"/>

    <xsl:variable name="month.name">
      <xsl:choose>
        <xsl:when test="$month=1">January</xsl:when>
        <xsl:when test="$month=2">February</xsl:when>
        <xsl:when test="$month=3">March</xsl:when>
        <xsl:when test="$month=4">April</xsl:when>
        <xsl:when test="$month=5">May</xsl:when>
        <xsl:when test="$month=6">June</xsl:when>
        <xsl:when test="$month=7">July</xsl:when>
        <xsl:when test="$month=8">August</xsl:when>
        <xsl:when test="$month=9">September</xsl:when>
        <xsl:when test="$month=10">October</xsl:when>
        <xsl:when test="$month=11">November</xsl:when>
        <xsl:when test="$month=12">December</xsl:when>
      </xsl:choose>
    </xsl:variable>

    <xsl:value-of select="concat($month.name, ' ', $day, ', ', $year, ' at ',
                                 $time, ' GMT')"/>
  </xsl:template>


  <xsl:template name="format.svn.revision">
    <xsl:param name="text"/>

    <!-- Remove the "$Date: " -->
    <xsl:variable name="text.noprefix"
      select="substring-after($text, '$Date: ')"/>

    <!-- Grab the year -->
    <xsl:variable name="year" select="substring-before($text.noprefix, '-')"/>
    <xsl:variable name="text.noyear"
      select="substring-after($text.noprefix, '-')"/>

    <!-- Grab the month -->
    <xsl:variable name="month" select="substring-before($text.noyear, '-')"/>
    <xsl:variable name="text.nomonth"
      select="substring-after($text.noyear, '-')"/>

    <!-- Grab the year -->
    <xsl:variable name="day" select="substring-before($text.nomonth, ' ')"/>
    <xsl:variable name="text.noday"
      select="substring-after($text.nomonth, ' ')"/>

    <!-- Get the time -->
    <xsl:variable name="time" select="substring-before($text.noday, ' ')"/>
    <xsl:variable name="text.notime"
      select="substring-after($text.noday, ' ')"/>

    <!-- Get the timezone -->
    <xsl:variable name="timezone" select="substring-before($text.notime, ' ')"/>

    <xsl:variable name="month.name">
      <xsl:choose>
        <xsl:when test="$month=1">January</xsl:when>
        <xsl:when test="$month=2">February</xsl:when>
        <xsl:when test="$month=3">March</xsl:when>
        <xsl:when test="$month=4">April</xsl:when>
        <xsl:when test="$month=5">May</xsl:when>
        <xsl:when test="$month=6">June</xsl:when>
        <xsl:when test="$month=7">July</xsl:when>
        <xsl:when test="$month=8">August</xsl:when>
        <xsl:when test="$month=9">September</xsl:when>
        <xsl:when test="$month=10">October</xsl:when>
        <xsl:when test="$month=11">November</xsl:when>
        <xsl:when test="$month=12">December</xsl:when>
      </xsl:choose>
    </xsl:variable>

    <xsl:value-of select="concat($month.name, ' ', $day, ', ', $year, ' at ',
                                 $time, ' ', $timezone)"/>
  </xsl:template>

  <!-- Footer Copyright -->
  <xsl:template match="copyright" mode="boost.footer">
    <xsl:if test="position() &gt; 1">
      <br/>
    </xsl:if>
    <xsl:call-template name="gentext">
      <xsl:with-param name="key" select="'Copyright'"/>
    </xsl:call-template>
    <xsl:call-template name="gentext.space"/>
    <xsl:call-template name="dingbat">
      <xsl:with-param name="dingbat">copyright</xsl:with-param>
    </xsl:call-template>
    <xsl:call-template name="gentext.space"/>
    <xsl:call-template name="copyright.years">
      <xsl:with-param name="years" select="year"/>
      <xsl:with-param name="print.ranges" select="$make.year.ranges"/>
      <xsl:with-param name="single.year.ranges"
        select="$make.single.year.ranges"/>
    </xsl:call-template>
    <xsl:call-template name="gentext.space"/>
    <xsl:apply-templates select="holder" mode="titlepage.mode"/>
  </xsl:template>

  <!-- Footer License -->
  <xsl:template match="legalnotice" mode="boost.footer">
    <xsl:apply-templates select="para" mode="titlepage.mode" />
  </xsl:template>

  <xsl:template name="user.footer.content">
    <table width="100%">
      <tr>
        <td align="left">
          <xsl:variable name="revision-nodes"
            select="ancestor-or-self::*
                    [not (attribute::rev:last-revision='')]"/>
          <xsl:if test="count($revision-nodes) &gt; 0">
            <xsl:variable name="revision-node"
              select="$revision-nodes[last()]"/>
            <xsl:variable name="revision-text">
              <xsl:value-of
                select="normalize-space($revision-node/attribute::rev:last-revision)"/>
            </xsl:variable>
            <xsl:if test="string-length($revision-text) &gt; 0">
              <p>
                <small>
                  <xsl:text>Last revised: </xsl:text>
                  <xsl:choose>
                    <xsl:when test="contains($revision-text, '/')">
                      <xsl:call-template name="format.cvs.revision">
                        <xsl:with-param name="text" select="$revision-text"/>
                      </xsl:call-template>
                    </xsl:when>
                    <xsl:otherwise>
                      <xsl:call-template name="format.svn.revision">
                        <xsl:with-param name="text" select="$revision-text"/>
                      </xsl:call-template>
                    </xsl:otherwise>
                  </xsl:choose>
                </small>
              </p>
            </xsl:if>
          </xsl:if>
        </td>
        <td align="right">
          <div class = "copyright-footer">
            <xsl:apply-templates select="ancestor::*/*/copyright"
              mode="boost.footer"/>
            <xsl:apply-templates select="ancestor::*/*/legalnotice"
              mode="boost.footer"/>
          </div>
        </td>
      </tr>
    </table>
  </xsl:template>

  <!-- We don't want refentry's to show up in the TOC because they
       will merely be redundant with the synopsis. -->
  <xsl:template match="refentry" mode="toc"/>

  <!-- override the behaviour of some DocBook elements for better
       rendering facilities -->

  <xsl:template match = "programlisting[ancestor::informaltable]">
     <pre class = "table-{name(.)}"><xsl:apply-templates/></pre>
  </xsl:template>

  <xsl:template match = "refsynopsisdiv">
     <h2 class = "{name(.)}-title">Synopsis</h2>
     <div class = "{name(.)}">
        <xsl:apply-templates/>
     </div>
  </xsl:template>

<!-- ============================================================ -->

<xsl:template name="output.html.stylesheets">
    <xsl:param name="stylesheets" select="''"/>

    <xsl:choose>
        <xsl:when test="contains($stylesheets, ' ')">
            <link rel="stylesheet">
                <xsl:attribute name="href">
                    <xsl:call-template name="href.target.relative">
                        <xsl:with-param name="target" select="substring-before($stylesheets, ' ')"/>
                    </xsl:call-template>
                </xsl:attribute>
                <xsl:if test="$html.stylesheet.type != ''">
                    <xsl:attribute name="type">
                        <xsl:value-of select="$html.stylesheet.type"/>
                    </xsl:attribute>
                </xsl:if>
            </link>
            <xsl:call-template name="output.html.stylesheets">
                <xsl:with-param name="stylesheets" select="substring-after($stylesheets, ' ')"/>
            </xsl:call-template>
        </xsl:when>
        <xsl:when test="$stylesheets != ''">
            <link rel="stylesheet">
                <xsl:attribute name="href">
                    <xsl:call-template name="href.target.relative">
                        <xsl:with-param name="target" select="$stylesheets"/>
                    </xsl:call-template>
                </xsl:attribute>
                <xsl:if test="$html.stylesheet.type != ''">
                    <xsl:attribute name="type">
                        <xsl:value-of select="$html.stylesheet.type"/>
                    </xsl:attribute>
                </xsl:if>
            </link>
        </xsl:when>
    </xsl:choose>
</xsl:template>

</xsl:stylesheet>
