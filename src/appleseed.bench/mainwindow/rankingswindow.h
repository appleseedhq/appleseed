
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

#pragma once

// appleseed.bench headers.
#include "utility/windowbase.h"

// Qt headers.
#include <QNetworkAccessManager>
#include <QObject>
#include <QString>

// Forward declarations.
namespace Ui { class RankingsWindow; }
class QCloseEvent;
class QNetworkReply;
class QWidget;

namespace appleseed {
namespace bench {

//
// Rankings window.
//

class RankingsWindow
  : public WindowBase
{
    Q_OBJECT

  public:
    // Constructors.
    explicit RankingsWindow(QWidget* parent = nullptr);
    RankingsWindow(const QString& user_result_id, QWidget* parent = nullptr);

    // Destructor.
    ~RankingsWindow() override;

    void closeEvent(QCloseEvent* event) override;

  signals:
    void signal_application_settings_modified() const;

  private:
    class RenderTimeItemDelegate;

    // Not wrapped in std::unique_ptr<> to avoid pulling in the UI definition code.
    Ui::RankingsWindow*     m_ui;

    QString                 m_user_result_id;

    QNetworkAccessManager   m_network_access_manager;
    QNetworkReply*          m_reply = nullptr;

    RenderTimeItemDelegate* m_render_time_item_delegate;

    void build_connections();
    void query_results();

  private slots:
    void slot_results_query_download_progress(const qint64 bytes_received, const qint64 bytes_total);
    void slot_results_query_finished();

    void slot_filter_text_changed(const QString& pattern);
    void slot_clear_filter();
};

}   // namespace bench
}   // namespace appleseed
