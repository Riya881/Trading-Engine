// Auto-Trading Engine for 5 Company Stocks
// trades every 5 minutes for 6 hours (72 times/day)
// moving average for buy/sell signals
// Buys/sells based on limit price checks
// clear all position at day end
// Black-Scholes model for option pricing
// every 10-min interval check for forecasted price drop to sell shares
// also liquidates in-the-money options early based on expected value

#include <iostream>
#include <vector>
#include <deque>
#include <cmath>
#include <numeric>
#include <iomanip>
#include <map>
#include <string>
#include <cstdlib>
#include <ctime>
using namespace std;

const int TICKS_PER_DAY = 72;
const int COMPANIES = 5;
const int SMA_WINDOW = 10; // 10-tick Simple Moving Average
const double LIMIT_SLIPPAGE = 0.01;//Slippage is the difference between the expected priceand actual price of trade
const double INITIAL_BALANCE = 100000.0;

double normal_cdf(double x) { 
    return 0.5 * erfc(-x / sqrt(2));
    }

double call_price(double S, double K, double T, double r, double sigma) {
    if (sigma == 0) return max(0.0, S - K);
    double d1 = (log(S / K) + (r + 0.5 * sigma * sigma) * T) / (sigma * sqrt(T));
    double d2 = d1 - sigma * sqrt(T);
    return S * normal_cdf(d1) - K * exp(-r * T) * normal_cdf(d2);
}

double put_price(double S, double K, double T, double r, double sigma) {
    if (sigma == 0) return max(0.0, K - S);
    double d1 = (log(S / K) + (r + 0.5 * sigma * sigma) * T) / (sigma * sqrt(T));
    double d2 = d1 - sigma * sqrt(T);
    return K * exp(-r * T) * normal_cdf(-d2) - S * normal_cdf(-d1);
}

struct OptionContract {
    double strike;
    double premium;
    double timeToMaturity;
    bool isCall; // true for call, false for put
};

struct Position {//Stores information about owned stocks and associated options per company.
    string company;
    int shares = 0;
    double avgPrice = 0.0;
    double optionPayout = 0.0;
    vector<OptionContract> optionsHeld;
};

class TradingEngine {
    map<string, deque<double>> history;//for storing the history of companies for calculating SMA
    map<string, Position> portfolio;// how many positions are held of which companies in the portfolio
    double balance;

public:
    TradingEngine(double startBalance) : balance(startBalance) {
        cout << fixed << setprecision(2);
        cout << "Initial Balance: $" << balance << endl;
    }

    void update_price(const string& company, double price, int tick) {
        auto& hist = history[company];
        hist.push_back(price);
        if (hist.size() > SMA_WINDOW) hist.pop_front();

        if (hist.size() == SMA_WINDOW) {
            double sma = accumulate(hist.begin(), hist.end(), 0.0) / SMA_WINDOW;
            double limitBuy = price * (1.0 - LIMIT_SLIPPAGE);//you place a buy order only if it’s ≤ limitBuy
            double limitSell = price * (1.0 + LIMIT_SLIPPAGE);

            if (price < sma && balance >= limitBuy) {
                int qty = int(balance / limitBuy / COMPANIES);// how many shares we can buy is qty
                if (qty > 0) {
                    balance -= qty * limitBuy;
                    auto& pos = portfolio[company];
                    pos.avgPrice = (pos.avgPrice * pos.shares + limitBuy * qty) / (pos.shares + qty);
                    pos.shares += qty;
                    cout << "BUY " << qty << " shares of " << company << " at $" << limitBuy << endl;

                    double strike = price * 1.05;//strike price for the call option to be 5% higher placing an OTM call
                    double callPremium = call_price(price, strike, 0.1, 0.01, 0.2);
                    if (balance >= callPremium) {
                        balance -= callPremium;
                        OptionContract opt = {strike, callPremium, 0.1};
                        pos.optionsHeld.push_back(opt);
                        cout << "BUY CALL OPTION on " << company << " strike: $" << strike << " premium: $" << callPremium << endl;
                    }
                    
                    double putStrike = price * 0.95;
                    double putPremium = put_price(price, putStrike, 0.1, 0.01, 0.2);
                    if (balance >= putPremium) {
                        balance -= putPremium;
                        OptionContract opt = {putStrike, putPremium, 0.1, false};
                        pos.optionsHeld.push_back(opt);
                        cout << "BUY PUT OPTION on " << company << " strike: $" << putStrike << " premium: $" << putPremium << endl;
                    }
                }
            }

            auto& pos = portfolio[company];
            if (price > sma && pos.shares > 0 && price > pos.avgPrice * 1.01) {
                balance += pos.shares * limitSell;
                cout << "SELL " << pos.shares << " shares of " << company << " at $" << limitSell << endl;
                pos.shares = 0;
                pos.avgPrice = 0;
            }

            if (tick % 2 == 0 && pos.shares > 0 && price < sma * 0.97) {//tick%2==0 runs every 10 min tick=5min
                balance += pos.shares * price;
                cout << "ALERT SELL " << pos.shares << " shares of " << company << " at $" << price << " due to drop forecast" << endl;
                pos.shares = 0;
                pos.avgPrice = 0;
            }

            if (tick % 2 == 0) {
                vector<OptionContract> remainingOptions;
                for (auto& opt : pos.optionsHeld) {
                    if ((opt.isCall && price > opt.strike) || (!opt.isCall && price < opt.strike)) {
                        double payout = opt.isCall ? price - opt.strike : opt.strike - price;
                        balance += payout;
                        cout << "ALERT EXIT " << (opt.isCall ? "CALL" : "PUT") << " OPTION on " << company << " payout: $" << payout << endl;
                    } else {
                        remainingOptions.push_back(opt);
                    }
                }
                pos.optionsHeld = remainingOptions;
            }
        }
    }

    void end_of_day_settlement(map<string, double>& lastPrices) {
        for (auto& [company, pos] : portfolio) {
            if (pos.shares > 0) {
                cout << "EOD SELL " << pos.shares << " shares of " << company << " at $" << lastPrices[company] << endl;
                balance += pos.shares * lastPrices[company];
                pos.shares = 0;
                pos.avgPrice = 0;
            }
            for (auto& opt : pos.optionsHeld) {
                if ((opt.isCall && lastPrices[company] > opt.strike) || (!opt.isCall && lastPrices[company] < opt.strike)) {
                    double payout = opt.isCall ? lastPrices[company] - opt.strike : opt.strike - lastPrices[company];
                    balance += payout;
                    cout << "OPTION PAYOUT for " << company << " strike $" << opt.strike << ": $" << payout << endl;
                }
            }
            pos.optionsHeld.clear();
        }
    }

    void print_summary(double initialBalance) const {
        cout << fixed << setprecision(2);
        cout << "Final Balance: $" << balance << endl;
        double profitLoss = balance - initialBalance;
        cout << (profitLoss >= 0 ? "Profit: $" : "Loss: $") << abs(profitLoss) << endl;
        for (const auto& [company, pos] : portfolio) {
            if (pos.shares > 0) {
                cout << company << ": " << pos.shares << " shares held at avg $" << pos.avgPrice << endl;
            }
        }
    }
};

int main() {
    srand(time(0));
    TradingEngine engine(INITIAL_BALANCE);
    vector<string> companies = {"AAPL", "GOOGL", "AMZN", "MSFT", "TSLA"};
    map<string, double> prices;

    for (int tick = 0; tick < TICKS_PER_DAY; ++tick) {
        for (const string& company : companies) {
            double priceChange = ((rand() % 201) - 100) / 1000.0;
            if (prices.find(company) == prices.end()) {
                prices[company] = 100 + rand() % 50;
            }
            prices[company] *= (1 + priceChange);
            prices[company] = round(prices[company] * 100.0) / 100.0;
            engine.update_price(company, prices[company], tick);
        }
    }

    engine.end_of_day_settlement(prices);
    engine.print_summary(INITIAL_BALANCE);
    return 0;
}