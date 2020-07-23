#pragma once

// #include <CoreHelpers.h>

#include <math.h>

class FilmicToneCurve
{
public:
	struct CurveParamsUser
	{
		CurveParamsUser()
		{
			m_toeStrength = 0.0f;
			m_toeLength = 0.5f;
			m_shoulderStrength = 0.0f; // white point
			m_shoulderLength = 0.5f;

			m_shoulderAngle = 0.0f;
			m_gamma = 1.0f;
		}
		float m_toeStrength; // as a ratio
		float m_toeLength; // as a ratio
		float m_shoulderStrength; // as a ratio
		float m_shoulderLength; // in F stops
		float m_shoulderAngle; // as a ratio

		float m_gamma;
	};

	struct CurveParamsDirect
	{
		CurveParamsDirect()
		{
			Reset();
		}

		void Reset()
		{
			m_x0 = .25f;
			m_y0 = .25f;
			m_x1 = .75f;
			m_y1 = .75f;
			m_W = 1.0f;

			m_gamma = 1.0f;

			m_overshootX = 0.0f;
			m_overshootY = 0.0f;
		}

		float m_x0;
		float m_y0;
		float m_x1;
		float m_y1;
		float m_W;

		float m_overshootX;
		float m_overshootY;

		float m_gamma;
	};

	struct CurveSegment
	{
		CurveSegment()
		{
			Reset();
		}

		void Reset()
		{
			m_offsetX = 0.0f;
			m_offsetY = 0.0f;
			m_scaleX = 1.0f; // always 1 or -1
			m_lnA = 0.0f;
			m_B = 1.0f;
		}

		float Eval(float x) const;
		float EvalInv(float y) const;

		float m_offsetX;
		float m_offsetY;
		float m_scaleX; // always 1 or -1
		float m_scaleY;
		float m_lnA;
		float m_B;
	};

	struct FullCurve
	{
		FullCurve()
		{
			Reset();
		}

		void Reset()
		{
			m_W = 1.0f;
			m_invW = 1.0f;

			m_x0 = .25f;
			m_y0 = .25f;
			m_x1 = .75f;
			m_y1 = .75f;


			for (int i = 0; i < 3; i++)
			{
				m_segments[i].Reset();
				m_invSegments[i].Reset();
			}
		}

		float Eval(float x) const;
		float EvalInv(float x) const;

		float m_W;
		float m_invW;

		float m_x0;
		float m_x1;
		float m_y0;
		float m_y1;


		CurveSegment m_segments[3];
		CurveSegment m_invSegments[3];
	};

	static void CreateCurve(FullCurve & dstCurve, const CurveParamsDirect & srcParams);
	static void CalcDirectParamsFromUser(CurveParamsDirect & dstParams, /*const*/ CurveParamsUser & srcParams);

};



