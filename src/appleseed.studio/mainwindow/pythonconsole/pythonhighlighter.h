
/*
$Id: KickPythonSyntaxHighlighter.h 167 2013-11-03 17:01:22Z oliver $
This is a C++ port of the following PyQt example
http://diotavelli.net/PyQtWiki/Python%20syntax%20highlighting
    C++ port by Frankie Simon (docklight.de, www.fuh-edv.de)

The following free software license applies for this file ("X11 license"):

Permission is hereby granted, free of charge, to any person obtaining a copy of this software
    and associated documentation files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial
    portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
    LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
    ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

#ifndef APPLESEED_STUDIO_MAINWINDOW_PYTHONCONSOLE_PYTHONHIGHLIGHTER_H
#define APPLESEED_STUDIO_MAINWINDOW_PYTHONCONSOLE_PYTHONHIGHLIGHTER_H

#include <QSyntaxHighlighter>

namespace appleseed
{
namespace studio
{

// Container to describe a highlighting rule. Based on a regular expression, a relevant match # and the format.
class HighlightingRule
{
  public:
    HighlightingRule(const QString& patternStr, int n, const QTextCharFormat& matchingFormat)
    {
        originalRuleStr = patternStr;
        pattern = QRegExp(patternStr);
        nth = n;
        format = matchingFormat;
    }

    QString originalRuleStr;
    QRegExp pattern;
    int nth;
    QTextCharFormat format;
};

// Implementation of highlighting for Python code.
class PythonSyntaxHighlighter
  : public QSyntaxHighlighter
{
    Q_OBJECT

  public:
    PythonSyntaxHighlighter(QTextDocument* parent = nullptr);

  protected:
    void highlightBlock(const QString& text) override;

  private:
    QStringList keywords;
    QStringList operators;
    QStringList braces;

    QHash <QString, QTextCharFormat> basicStyles;

    void initializeRules();

    // Highlights multi-line strings, returns true if after processing we are still within the multi-line section.
    bool matchMultiline(const QString& text, const QRegExp& delimiter, const int inState, const QTextCharFormat& style);

    const QTextCharFormat getTextCharFormat(int r, int g, int b, const QString& style = QString());

    QList <HighlightingRule> rules;
    QRegExp triSingleQuote;
    QRegExp triDoubleQuote;
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PYTHONCONSOLE_PYTHONHIGHLIGHTER_H
