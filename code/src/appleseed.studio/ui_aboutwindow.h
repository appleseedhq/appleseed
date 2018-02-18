/********************************************************************************
** Form generated from reading UI file 'aboutwindow.ui'
**
** Created by: Qt User Interface Compiler version 4.8.7
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_ABOUTWINDOW_H
#define UI_ABOUTWINDOW_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QDialogButtonBox>
#include <QtGui/QFrame>
#include <QtGui/QGridLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_AboutWindow
{
public:
    QGridLayout *gridLayout;
    QGridLayout *layout_details;
    QDialogButtonBox *dialogbuttonbox;
    QLabel *label_details;
    QFrame *frame_banner;
    QGridLayout *gridLayout_3;
    QLabel *label_version_string;
    QLabel *label_product_image;

    void setupUi(QWidget *AboutWindow)
    {
        if (AboutWindow->objectName().isEmpty())
            AboutWindow->setObjectName(QString::fromUtf8("AboutWindow"));
        AboutWindow->setWindowModality(Qt::ApplicationModal);
        AboutWindow->resize(500, 340);
        gridLayout = new QGridLayout(AboutWindow);
        gridLayout->setSpacing(6);
        gridLayout->setContentsMargins(0, 0, 0, 0);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        layout_details = new QGridLayout();
        layout_details->setSpacing(6);
        layout_details->setObjectName(QString::fromUtf8("layout_details"));
        layout_details->setContentsMargins(20, 15, 20, 20);
        dialogbuttonbox = new QDialogButtonBox(AboutWindow);
        dialogbuttonbox->setObjectName(QString::fromUtf8("dialogbuttonbox"));
        dialogbuttonbox->setStandardButtons(QDialogButtonBox::Ok);

        layout_details->addWidget(dialogbuttonbox, 2, 0, 1, 1);

        label_details = new QLabel(AboutWindow);
        label_details->setObjectName(QString::fromUtf8("label_details"));
        QSizePolicy sizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(label_details->sizePolicy().hasHeightForWidth());
        label_details->setSizePolicy(sizePolicy);
        label_details->setAlignment(Qt::AlignLeading|Qt::AlignLeft|Qt::AlignTop);
        label_details->setOpenExternalLinks(true);
        label_details->setTextInteractionFlags(Qt::LinksAccessibleByKeyboard|Qt::LinksAccessibleByMouse);

        layout_details->addWidget(label_details, 1, 0, 1, 1);


        gridLayout->addLayout(layout_details, 2, 0, 1, 1);

        frame_banner = new QFrame(AboutWindow);
        frame_banner->setObjectName(QString::fromUtf8("frame_banner"));
        frame_banner->setFrameShape(QFrame::StyledPanel);
        frame_banner->setFrameShadow(QFrame::Raised);
        gridLayout_3 = new QGridLayout(frame_banner);
        gridLayout_3->setSpacing(6);
        gridLayout_3->setContentsMargins(20, 20, 20, 20);
        gridLayout_3->setObjectName(QString::fromUtf8("gridLayout_3"));
        label_version_string = new QLabel(frame_banner);
        label_version_string->setObjectName(QString::fromUtf8("label_version_string"));
        QSizePolicy sizePolicy1(QSizePolicy::Preferred, QSizePolicy::MinimumExpanding);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(label_version_string->sizePolicy().hasHeightForWidth());
        label_version_string->setSizePolicy(sizePolicy1);
        QFont font;
        font.setPointSize(8);
        label_version_string->setFont(font);
        label_version_string->setAlignment(Qt::AlignLeading|Qt::AlignLeft|Qt::AlignTop);
        label_version_string->setTextInteractionFlags(Qt::LinksAccessibleByKeyboard|Qt::LinksAccessibleByMouse|Qt::TextBrowserInteraction|Qt::TextSelectableByKeyboard|Qt::TextSelectableByMouse);

        gridLayout_3->addWidget(label_version_string, 2, 0, 1, 1);

        label_product_image = new QLabel(frame_banner);
        label_product_image->setObjectName(QString::fromUtf8("label_product_image"));
        QSizePolicy sizePolicy2(QSizePolicy::Preferred, QSizePolicy::Fixed);
        sizePolicy2.setHorizontalStretch(0);
        sizePolicy2.setVerticalStretch(0);
        sizePolicy2.setHeightForWidth(label_product_image->sizePolicy().hasHeightForWidth());
        label_product_image->setSizePolicy(sizePolicy2);
        label_product_image->setStyleSheet(QString::fromUtf8("QLabel\n"
"{\n"
"    margin-bottom: 15px;\n"
"}"));
        label_product_image->setPixmap(QPixmap(QString::fromUtf8(":/images/appleseed-logo-256.png")));

        gridLayout_3->addWidget(label_product_image, 1, 0, 1, 1);


        gridLayout->addWidget(frame_banner, 0, 0, 1, 1);


        retranslateUi(AboutWindow);

        QMetaObject::connectSlotsByName(AboutWindow);
    } // setupUi

    void retranslateUi(QWidget *AboutWindow)
    {
        AboutWindow->setWindowTitle(QApplication::translate("AboutWindow", "About appleseed.studio", 0, QApplication::UnicodeUTF8));
        label_details->setText(QApplication::translate("AboutWindow", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span>Copyright \302\251 2010-2013 Fran\303\247ois Beaune, Jupiter Jazz Limited.</span></p>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span>Copyright \302\251 2014-2017 The appleseedhq Organization.</span></p>\n"
"<p style=\"-qt-paragraph-type:empty; margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><br /></p>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span>This software is released under the </span><a href=\"http://opensource"
                        ".org/licenses/MIT\"><span style=\" text-decoration: underline; color:#be8c32;\">MIT license</span></a><span>.</span></p>\n"
"<p style=\"-qt-paragraph-type:empty; margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><br /></p>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span>Visit </span><a href=\"http://appleseedhq.net/\"><span style=\"text-decoration: underline; color:#be8c32;\">http://appleseedhq.net/</span></a><span> for additional information and resources.</span></p></body></html>", 0, QApplication::UnicodeUTF8));
        frame_banner->setStyleSheet(QApplication::translate("AboutWindow", "QFrame\n"
"{\n"
"	background-color: rgb(44, 44, 44);\n"
"	border-left: 0px;\n"
"	border-top: 0px;\n"
"	border-right: 0px;\n"
"}\n"
"", 0, QApplication::UnicodeUTF8));
        label_version_string->setText(QApplication::translate("AboutWindow", "Version String", 0, QApplication::UnicodeUTF8));
        label_product_image->setText(QString());
    } // retranslateUi

};

namespace Ui {
    class AboutWindow: public Ui_AboutWindow {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_ABOUTWINDOW_H
