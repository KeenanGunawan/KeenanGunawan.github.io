#!/usr/bin/env python3
"""Mortgage protection insurance pricing and sensitivity analysis.

No third-party packages are required. Run with:
    python3 code/mortgage-protection-pricing.py
"""

import math

ALPHA = -10.4
BETA = 0.0816
EPSILON = -7.6
KAPPA = -0.01
ISSUE_AGE = 35
RISK_DISCOUNT_RATE = 0.05
TARGET_MARGIN = 0.12
MONTHLY_DISCOUNT = (1 + RISK_DISCOUNT_RATE) ** (-1 / 12)
MONTHLY_LAPSE = 1 - (1 - 0.015) ** (1 / 12)
MONTHLY_PREPAYMENT = 1 - (1 - 0.02) ** (1 / 12)

OPTIONS = {
    "A": {"loan": 600_000, "rate": 0.065, "years": 20},
    "B": {"loan": 600_000, "rate": 0.075, "years": 25},
    "C": {"loan": 700_000, "rate": 0.085, "years": 25},
}


def cumulative_hazard(age, duration):
    """Closed-form integrated Makeham hazard from 0 to duration."""
    background = (
        math.exp(EPSILON)
        * (math.exp(KAPPA * duration) - 1)
        / KAPPA
    )
    ageing = (
        math.exp(ALPHA + BETA * age)
        * (math.exp((BETA + KAPPA) * duration) - 1)
        / (BETA + KAPPA)
    )
    return background + ageing


def survival(age, duration):
    return math.exp(-cumulative_hazard(age, duration))


def independent_monthly_death(age, month, shock=1.0):
    start = (month - 1) / 12
    end = month / 12
    increment = cumulative_hazard(age, end) - cumulative_hazard(age, start)
    return min(1.0, (1 - math.exp(-increment)) * shock)


def dependent_decrements(independent_death):
    """Convert independent monthly rates to dependent UDD decrements."""
    qd = independent_death * (
        1
        - MONTHLY_LAPSE / 2
        - MONTHLY_PREPAYMENT / 2
        + MONTHLY_LAPSE * MONTHLY_PREPAYMENT / 3
    )
    ql = MONTHLY_LAPSE * (
        1
        - independent_death / 2
        - MONTHLY_PREPAYMENT / 2
        + independent_death * MONTHLY_PREPAYMENT / 3
    )
    qp = MONTHLY_PREPAYMENT * (
        1
        - independent_death / 2
        - MONTHLY_LAPSE / 2
        + independent_death * MONTHLY_LAPSE / 3
    )
    return qd, ql, qp


def life_table_value(age):
    """Return the five requested annual actuarial values for one age."""
    p = [survival(age, k) for k in range(151)]
    v = 1 / 1.05
    assurance = sum(v ** (k + 1) * (p[k] - p[k + 1]) for k in range(150))
    annuity_due = sum(v**k * p[k] for k in range(150))
    assurance_second_moment = sum(
        v ** (2 * (k + 1)) * (p[k] - p[k + 1]) for k in range(150)
    )
    decreasing_assurance = sum(
        (30 - k) * v ** (k + 1) * (p[k] - p[k + 1])
        for k in range(30)
    )
    decreasing_annuity = sum((30 - k) * v**k * p[k] for k in range(30))
    return (
        assurance,
        annuity_due,
        assurance_second_moment,
        decreasing_assurance,
        decreasing_annuity,
    )


def project_option(loan, rate, years, premium=None, mortality_shock=1.0):
    """Price or test one mortgage protection option on an expected-PV basis."""
    months = years * 12
    monthly_rate = rate / 12
    repayment = loan * monthly_rate / (1 - (1 + monthly_rate) ** (-months))

    balance = loan
    in_force = 1.0
    premium_annuity = 0.0
    commission_factor = 0.0
    fixed_expense_pv = 0.0
    benefit_pv = 0.0
    monthly_rows = []

    for month in range(1, months + 1):
        independent_death = independent_monthly_death(
            ISSUE_AGE, month, mortality_shock
        )
        qd, ql, qp = dependent_decrements(independent_death)
        ending_balance = balance * (1 + monthly_rate) - repayment
        discount_start = MONTHLY_DISCOUNT ** (month - 1)
        discount_end = MONTHLY_DISCOUNT**month

        premium_annuity += in_force * discount_start
        if month == 1:
            fixed_expense_pv += 600 * in_force * discount_start
            commission_factor += 0.15 * in_force * discount_start
        elif (month - 1) % 12 == 0:
            fixed_expense_pv += 50 * in_force * discount_start
            commission_factor += 0.01 * in_force * discount_start

        benefit_pv += ending_balance * qd * in_force * discount_end
        monthly_rows.append(
            (month, in_force, qd, ql, qp, balance, ending_balance)
        )
        in_force *= 1 - qd - ql - qp
        balance = ending_balance

    if premium is None:
        premium = (benefit_pv + fixed_expense_pv) / (
            premium_annuity * (1 - TARGET_MARGIN) - commission_factor
        )

    epv_premiums = premium * premium_annuity
    npv_profit = (
        premium * premium_annuity
        - premium * commission_factor
        - fixed_expense_pv
        - benefit_pv
    )

    gross_cashflows = []
    for month, al, qd, _, _, _, ending_balance in monthly_rows:
        expense = 0.0
        if month == 1:
            expense = (600 + 0.15 * premium) * al
        elif (month - 1) % 12 == 0:
            expense = (50 + 0.01 * premium) * al
        benefit = ending_balance * qd * al
        gross_cashflows.append((premium * al - expense) / MONTHLY_DISCOUNT - benefit)

    return {
        "monthly_premium": premium,
        "annual_premium": 12 * premium,
        "monthly_repayment": repayment,
        "epv_premiums": epv_premiums,
        "npv_profit": npv_profit,
        "profit_margin": npv_profit / epv_premiums,
        "negative_months": sum(value < -1e-9 for value in gross_cashflows),
        "minimum_gross_cashflow": min(gross_cashflows),
    }


def main():
    print("BASE PRICING")
    baseline = {}
    for name, assumptions in OPTIONS.items():
        result = project_option(**assumptions)
        baseline[name] = result
        print(
            name,
            f"monthly=${result['monthly_premium']:.2f}",
            f"annual=${result['annual_premium']:.0f}",
            f"margin={result['profit_margin']:.2%}",
            f"negative gross-CF months={result['negative_months']}",
        )

    print("\nMORTALITY STRESS: PREMIUMS HELD FIXED")
    for shock in (1.05, 1.10):
        values = []
        for name, assumptions in OPTIONS.items():
            result = project_option(
                **assumptions,
                premium=baseline[name]["monthly_premium"],
                mortality_shock=shock,
            )
            values.append(f"{name}={result['profit_margin']:.2%}")
        print(f"{shock - 1:.0%} shock:", ", ".join(values))

    print("\nRATE-ONLY SENSITIVITY: OPTION B LOAN, TERM AND PREMIUM HELD FIXED")
    fixed_premium = baseline["B"]["monthly_premium"]
    for rate in (0.065, 0.075, 0.085, 0.095):
        result = project_option(600_000, rate, 25, premium=fixed_premium)
        repriced = project_option(600_000, rate, 25)
        print(
            f"rate={rate:.1%}",
            f"fixed-premium margin={result['profit_margin']:.2%}",
            f"required annual premium=${repriced['annual_premium']:.2f}",
        )

    print("\nLIFE TABLE CHECKPOINTS")
    for age in (30, 35, 65, 99):
        values = "  ".join(f"{value:.3f}" for value in life_table_value(age))
        print(age, values)


if __name__ == "__main__":
    main()
