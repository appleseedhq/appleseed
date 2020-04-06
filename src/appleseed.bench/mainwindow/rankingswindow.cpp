
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Francois Beaune, The appleseedhq Organization
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//

// Interface header.
#include "rankingswindow.h"

// UI definition header.
#include "ui_rankingswindow.h"

// appleseed.bench headers.
#include "mainwindow/constants.h"
#include "utility/backendapi.h"
#include "utility/formatrendertime.h"
#include "utility/systeminfo.h"

// appleseed.qtcommon headers.
#include "utility/miscellaneous.h"

// appleseed.renderer headers.
#include "renderer/api/log.h"

// Qt headers.
#include <QByteArray>
#include <QChar>
#include <QDateTime>
#include <QDialogButtonBox>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonValue>
#include <QMessageBox>
#include <QNetworkReply>
#include <QNetworkRequest>
#include <QPainter>
#include <QShortcut>
#include <QStringMatcher>
#include <QStyledItemDelegate>
#include <Qt>
#include <QUrl>

// Standard headers.
#include <algorithm>
#include <cassert>

using namespace appleseed::qtcommon;

namespace appleseed {
namespace bench {

namespace
{
    constexpr int CpuModelColumnIndex = 0;
    constexpr int EnabledCpuCoreCountColumnIndex = 1;
    constexpr int CpuThreadCountColumnIndex = 2;
    constexpr int RenderThreadCountColumnIndex = 3;
    constexpr int RenderTimeColumnIndex = 4;

    enum class ResultType
    {
        UserSubmittedResult = 0,
        AMDCPU = 1,
        IntelCPU = 2,
        UnknownCPU = 3
    };

    constexpr int ResultTypeRole = Qt::UserRole + 0;
    constexpr int RenderTimeRole = Qt::UserRole + 1;

    const QColor UserSubmittedResultItemColor(190, 140, 50);
    const QColor DefaultResultItemColor(60, 60, 60);

    const QColor AMDCPUColor(255, 90, 90);
    const QColor IntelCPUColor(0, 160, 255);
}


//
// RankingsWindow::RenderTimeItemDelegate class implementation.
//

class RankingsWindow::RenderTimeItemDelegate
  : public QStyledItemDelegate
{
  public:
    RenderTimeItemDelegate(QTableWidget* table_widget)
      : QStyledItemDelegate(table_widget)
      , m_table_widget(table_widget)
    {
    }

    void set_max_render_time(const int max_render_time)
    {
        m_max_render_time = max_render_time;
    }

    void paint(QPainter* painter,
               const QStyleOptionViewItem& option,
               const QModelIndex& index) const override
    {
        assert(index.isValid());
        assert(index.column() == RenderTimeColumnIndex);

        if (m_max_render_time > 0)
        {
            const QTableWidgetItem* item = m_table_widget->item(index.row(), index.column());
            const auto result_type = static_cast<ResultType>(item->data(ResultTypeRole).toInt());
            const int render_time = item->data(RenderTimeRole).toInt();
            const float fraction = static_cast<float>(render_time) / m_max_render_time;

            QRect rect(option.rect);
            rect.setWidth(static_cast<int>(rect.width() * fraction));

            painter->save();
            painter->setPen(Qt::NoPen);
            painter->setBrush(
                result_type == ResultType::UserSubmittedResult
                    ? UserSubmittedResultItemColor
                    : DefaultResultItemColor);
            painter->drawRect(rect);
            painter->restore();
        }

        QStyledItemDelegate::paint(painter, option, index);
    }

  private:
    QTableWidget*   m_table_widget;
    int             m_max_render_time = 0;
};


//
// RankingsWindow class implementation.
//

RankingsWindow::RankingsWindow(QWidget* parent)
  : RankingsWindow(QString(), parent)
{
}

RankingsWindow::RankingsWindow(const QString& user_result_id, QWidget* parent)
  : WindowBase(parent, "rankings_window")
  , m_ui(new Ui::RankingsWindow())
  , m_user_result_id(user_result_id)
  , m_network_access_manager(this)
{
    m_ui->setupUi(this);

    setWindowFlags(Qt::Window);

    m_render_time_item_delegate = new RenderTimeItemDelegate(m_ui->tablewidget_results);

    m_ui->tablewidget_results->horizontalHeader()->setSectionResizeMode(CpuModelColumnIndex, QHeaderView::Interactive);
    m_ui->tablewidget_results->horizontalHeader()->setSectionResizeMode(EnabledCpuCoreCountColumnIndex, QHeaderView::Interactive);
    m_ui->tablewidget_results->horizontalHeader()->setSectionResizeMode(CpuThreadCountColumnIndex, QHeaderView::Interactive);
    m_ui->tablewidget_results->horizontalHeader()->setSectionResizeMode(RenderThreadCountColumnIndex, QHeaderView::Interactive);
    m_ui->tablewidget_results->horizontalHeader()->setSectionResizeMode(RenderTimeColumnIndex, QHeaderView::Stretch);
    m_ui->tablewidget_results->setColumnWidth(CpuModelColumnIndex, 500);
    m_ui->tablewidget_results->setItemDelegateForColumn(RenderTimeColumnIndex, m_render_time_item_delegate);
    m_ui->tablewidget_results->sortItems(RenderTimeColumnIndex);

    m_ui->frame_results->hide();

    build_connections();

    WindowBase::load_settings();

    query_results();
}

RankingsWindow::~RankingsWindow()
{
    delete m_ui;
}

void RankingsWindow::closeEvent(QCloseEvent* event)
{
    WindowBase::closeEvent(event);

    if (m_reply != nullptr)
        m_reply->abort();
}

void RankingsWindow::build_connections()
{
    connect(m_ui->buttonbox, &QDialogButtonBox::rejected, this, &RankingsWindow::close);

    connect(
        create_window_local_shortcut(this, Qt::Key_Escape), &QShortcut::activated,
        this, &RankingsWindow::close);

    connect(
        m_ui->pushbutton_download_cancel, &QPushButton::clicked,
        this, &RankingsWindow::close);

    connect(
        m_ui->lineedit_results_filter, &QLineEdit::textChanged,
        this, &RankingsWindow::slot_filter_text_changed);

    connect(
        create_window_local_shortcut(this, Qt::CTRL + Qt::Key_F), &QShortcut::activated,
        m_ui->lineedit_results_filter, [=](){ m_ui->lineedit_results_filter->setFocus(Qt::ShortcutFocusReason); });

    connect(
        m_ui->pushbutton_results_clear_filter, &QPushButton::clicked,
        this, &RankingsWindow::slot_clear_filter);

    m_ui->pushbutton_results_clear_filter->setEnabled(false);
}

void RankingsWindow::query_results()
{
    const QNetworkRequest request = make_api_request("/results");

    RENDERER_LOG_DEBUG("querying %s...", request.url().toString().toLocal8Bit().constData());

    m_reply = m_network_access_manager.get(request);
    assert(m_reply != nullptr);

    connect(
        m_reply, &QNetworkReply::downloadProgress,
        this, &RankingsWindow::slot_results_query_download_progress);

    connect(
        m_reply, &QNetworkReply::finished,
        this, &RankingsWindow::slot_results_query_finished);
}

void RankingsWindow::slot_results_query_download_progress(const qint64 bytes_received, const qint64 bytes_total)
{
    if (bytes_total > 0)
    {
        if (m_ui->progressbar_download->maximum() < bytes_total)
            m_ui->progressbar_download->setMaximum(static_cast<int>(bytes_total));

        m_ui->progressbar_download->setValue(bytes_received);
    }
}

void RankingsWindow::slot_results_query_finished()
{
    if (m_reply->error() == QNetworkReply::NoError)
    {
        RENDERER_LOG_DEBUG("query was successful.");

        try
        {
            const QByteArray reply_bytes = m_reply->readAll();
            // Keep the line below (commented), it's useful for debugging:
            // const QString reply_string(reply_bytes);
            const QJsonDocument json_doc = QJsonDocument::fromJson(reply_bytes);

            if (!json_doc.isArray())
                throw ExceptionJsonValue();

            int max_render_time = 0;
            QTableWidgetItem* user_result_item = nullptr;

            for (const QJsonValue& json_result : json_doc.array())
            {
                if (!json_result.isObject())
                    throw ExceptionJsonValue();

                const QJsonObject json_result_obj = json_result.toObject();

                BenchmarkResult result;
                try
                {
                    result = BenchmarkResult::from_json_object(json_result_obj);
                }
                catch (const ExceptionJsonValue&)
                {
                    const QJsonDocument json_doc(json_result_obj);
                    const QByteArray json = json_doc.toJson();
                    RENDERER_LOG_WARNING("skipping malformed benchmark result:\n%s", json.constData());
                    continue;
                }

                // Skip results for other versions of the core engine (anything that would impact performance).
                if (result.m_benchmark_version != BenchmarkVersion)
                    continue;

#ifndef APPLESEED_DEBUG
                // Skip results for other benchmark scenes (such as the Cornell Box used in debug builds).
                if (result.m_benchmark_scene_id != BenchmarkSceneIdFetch1)
                    continue;
#endif

                const QString cleaned_up_cpu_model = SystemInfo::cleanup_cpu_model_string(result.m_cpu_model);
                const bool is_amd_cpu = cleaned_up_cpu_model.contains("amd", Qt::CaseInsensitive);
                const bool is_intel_cpu = cleaned_up_cpu_model.contains("intel", Qt::CaseInsensitive);
                const bool is_user_result = result.m_id == m_user_result_id;
                const ResultType result_type =
                    is_user_result ? ResultType::UserSubmittedResult :
                    is_amd_cpu ? ResultType::AMDCPU :
                    is_intel_cpu ? ResultType::IntelCPU :
                    ResultType::UnknownCPU;

                const int row = m_ui->tablewidget_results->rowCount();
                m_ui->tablewidget_results->insertRow(row);

                QTableWidgetItem* cpu_model_item = new QTableWidgetItem(cleaned_up_cpu_model);
                m_ui->tablewidget_results->setItem(row, CpuModelColumnIndex, cpu_model_item);

                QTableWidgetItem* enabled_cpu_core_count_item = new QTableWidgetItem();
                enabled_cpu_core_count_item->setData(Qt::EditRole, result.m_enabled_cpu_core_count);
                enabled_cpu_core_count_item->setTextAlignment(Qt::AlignHCenter | Qt::AlignVCenter);
                m_ui->tablewidget_results->setItem(row, EnabledCpuCoreCountColumnIndex, enabled_cpu_core_count_item);

                QTableWidgetItem* cpu_thread_count_item = new QTableWidgetItem();
                cpu_thread_count_item->setData(Qt::EditRole, result.m_cpu_thread_count);
                cpu_thread_count_item->setTextAlignment(Qt::AlignHCenter | Qt::AlignVCenter);
                m_ui->tablewidget_results->setItem(row, CpuThreadCountColumnIndex, cpu_thread_count_item);

                QTableWidgetItem* render_thread_count_item = new QTableWidgetItem();
                render_thread_count_item->setData(Qt::EditRole, result.m_render_thread_count);
                render_thread_count_item->setTextAlignment(Qt::AlignHCenter | Qt::AlignVCenter);
                m_ui->tablewidget_results->setItem(row, RenderThreadCountColumnIndex, render_thread_count_item);

                QTableWidgetItem* render_time_item = new QTableWidgetItem(format_render_time(result.m_render_time));
                render_time_item->setData(ResultTypeRole, static_cast<int>(result_type));
                render_time_item->setData(RenderTimeRole, result.m_render_time);
                render_time_item->setTextAlignment(Qt::AlignHCenter | Qt::AlignVCenter);
                m_ui->tablewidget_results->setItem(row, RenderTimeColumnIndex, render_time_item);

                if (result_type == ResultType::UserSubmittedResult)
                {
                    user_result_item = cpu_model_item;

                    const QColor White(255, 255, 255);
                    cpu_model_item->setForeground(White);
                    enabled_cpu_core_count_item->setForeground(White);
                    cpu_thread_count_item->setForeground(White);
                    render_thread_count_item->setForeground(White);
                    render_time_item->setForeground(White);
                }
                else
                {
                    if (result_type == ResultType::AMDCPU)
                        cpu_model_item->setForeground(AMDCPUColor);
                    else if (result_type == ResultType::IntelCPU)
                        cpu_model_item->setForeground(IntelCPUColor);
                }

                max_render_time = std::max(max_render_time, result.m_render_time);
            }

            m_render_time_item_delegate->set_max_render_time(max_render_time);

            m_ui->frame_download->hide();
            m_ui->frame_results->show();

            // This must be done after the results frame is shown.
            if (user_result_item != nullptr)
                m_ui->tablewidget_results->scrollToItem(user_result_item, QAbstractItemView::PositionAtCenter);

            m_ui->tablewidget_results->setFocus();
        }
        catch (const ExceptionJsonValue&)
        {
            QMessageBox msgbox(this);
            msgbox.setWindowTitle("Parse Error");
            msgbox.setIcon(QMessageBox::Critical);
            msgbox.setText(
                "An error occurred while parsing ranking results.\n\n"
                "Make sure you are using the latest version of appleseed.bench.");
            msgbox.setStandardButtons(QMessageBox::Ok);
            msgbox.exec();
            close();
        }
    }
    else if (m_reply->error() != QNetworkReply::OperationCanceledError)
    {
        QMessageBox msgbox(this);
        msgbox.setWindowTitle("Query Error");
        msgbox.setIcon(QMessageBox::Critical);
        msgbox.setText(
            QString("The following error occurred while retrieving ranking results: %1")
                .arg(filter_api_key(m_reply->errorString())));
        msgbox.setStandardButtons(QMessageBox::Ok);
        msgbox.exec();
        close();
    }

    m_reply->deleteLater();
    m_reply = nullptr;
}

void RankingsWindow::slot_filter_text_changed(const QString& pattern)
{
    m_ui->pushbutton_results_clear_filter->setEnabled(!pattern.isEmpty());

    const QStringMatcher matcher(pattern, Qt::CaseInsensitive);

    for (int row = 0, row_count = m_ui->tablewidget_results->rowCount(); row < row_count; ++row)
    {
        const QTableWidgetItem* cpu_model_item = m_ui->tablewidget_results->item(row, CpuModelColumnIndex);
        const bool visible = matcher.indexIn(cpu_model_item->text()) >= 0;
        m_ui->tablewidget_results->setRowHidden(row, !visible);
    }
}

void RankingsWindow::slot_clear_filter()
{
    m_ui->lineedit_results_filter->clear();
}

}   // namespace bench
}   // namespace appleseed

#include "mainwindow/moc_cpp_rankingswindow.cxx"
