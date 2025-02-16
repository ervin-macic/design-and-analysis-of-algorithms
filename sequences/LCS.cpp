#include <bits/stdc++.h>
using namespace std;
//old without tabulation:
int lcs( string p, string q, int m, int n ){ 
    if (m == 0 || n == 0) 
        return 0; 
    if (p[m-1] == q[n-1]) 
        return 1 + LCS(p, q, m-1, n-1); 
    else
        return max(LCS(p, q, m, n-1), LCS(p, q, m-1, n)); 
} 
//new with memoization:
int LCS(string p,string q, int m, int n){
    int memo[m + 1][n + 1];
    for(int i = 0; i < m; i++){
        for(int j = 0; j < n; j++){
            if(i == 0 or j == 0){
                memo[i][j] = 0;
            }else if(p[i - 1] == q[j - 1]){
                memo[i][j] = 1 + memo[i - 1][j - 1];
            }else{
                memo[i][j] = max(memo[i - 1][j], memo[i][j - 1]);
            }
        }
    }
    return memo[m][n];
}
int main(){ 
    string p, q;
    cin >> p >> q;
    int m = p.length();
    int n = q.length(); 
    cout<<"Length of LCS is "<< LCS( p, q, m, n ) << endl;
} 