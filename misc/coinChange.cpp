#include<bits/stdc++.h>
#define pb push_back
#define ll long long
#define mp make_pair
using namespace std;
int dp(int coins[], int k, int target){
    int memo[target + 1];
    for(int i = 1; i < target + 1; i++){
        memo[i] = INT_MAX;
    }
    memo[0] = 0;
    for (int i=1; i <= target; i++){
        for (int j = 0; j < k; j++)
          if (coins[j] <= i){
              int sub_res = memo[i-coins[j]];
              if (sub_res != INT_MAX and sub_res + 1 < memo[i])
                  memo[i] = sub_res + 1;
          }
    }
      if(memo[target]==INT_MAX){
          return -1;
      }
    return memo[target];
}
int main(){
ios_base::sync_with_stdio(false); cin.tie(NULL);
 int n;
 cin >> n;
 int coins[n];
 for(int i = 0; i < n; i++){
     cin >> coins[i];
 }
 int target;
 cin >> target;
 cout << dp(coins,n,target) << endl;
}