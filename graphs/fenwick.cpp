#include <bits/stdc++.h>

using namespace std;

struct BIT
{
    vector<long long> table;

    BIT() {}
    BIT(long long N)
    {
        table.assign(N + 1, 0);
    }
    BIT(long long N, vector<long long> arr)
    {
        table.assign(N + 1, 0);
        for (long long i = 0; i < arr.size(); i++)
            update(i, arr[i]);
    }

    void update(long long i, long long delta)
    {
        i++;
        while (i < table.size())
        {
            table[i] += delta;
            i += i & (~i + 1);
        }
    }

    long long getSum(long long i)
    {
        i++;
        long long s = 0;
        while (i > 0)
        {
            s += table[i];
            i -= i & (~i + 1);
        }
        return s;
    }

    long long rangeSum(long long l, long long r)
    {
        return getSum(r) - getSum(l - 1);
    }
};

int main()
{
    long long n, q;
    cin >> n >> q;

    vector<long long> a(n);
    BIT freq(100001);
    for (long long i = 0; i < n; i++) {
        cin >> a[i];
        freq.update(a[i], 1);
    }

    BIT bit(n, a);
    for (long long j = 0; j < q; j++)
    {
        char o;
        cin >> o;

        if (o == 'C') {
            long long x, v;
            cin >> x >> v;
            x--;

            bit.update(x, v - a[x]);
            freq.update(a[x], -1);
            a[x] = v;
            freq.update(a[x], 1);
        }

        if (o == 'S') {
            long long l, r;
            cin >> l >> r;
            l--; r--;

            cout << bit.rangeSum(l, r) << endl;
        }

        if (o == 'Q') {
            long long v;
            cin >> v;

            cout << freq.getSum(v) << endl;
        }
    }

    return 0;
}