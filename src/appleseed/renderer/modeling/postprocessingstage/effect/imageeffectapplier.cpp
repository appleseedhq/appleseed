
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2020 Tiago Chaves, The appleseedhq Organization
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
#include "imageeffectapplier.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/modeling/postprocessingstage/effect/imageeffectjob.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/image/image.h"
#include "foundation/utility/job/ijob.h"
#include "foundation/utility/job/jobmanager.h"
#include "foundation/utility/job/jobqueue.h"

using namespace foundation;

namespace renderer
{

//
// ImageEffectApplier class implementation.
//

void ImageEffectApplier::apply_on_tiles(
    Image&              image,
    const std::size_t   thread_count) const
{
    // Create effect applier jobs.
    const ImageEffectJobFactory effect_job_factory;
    const ImageEffectJobFactory::EffectJobVector effect_jobs =
        effect_job_factory.create(
            image,
            *this);

    // Schedule effect applier jobs.
    JobQueue job_queue;
    for (ImageEffectJob* const effect_job : effect_jobs)
        job_queue.schedule(effect_job);

    // Create a job manager and wait until jobs have effectively stopped.
    JobManager job_manager(
        global_logger(),
        job_queue,
        thread_count);

    job_manager.start();
    job_queue.wait_until_completion();
}

}   // namespace renderer
