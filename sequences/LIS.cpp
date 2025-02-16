#include<bits/stdc++.h> 
using namespace std;
int LIS(int arr[], int n){
    //tabulation|memoization
    int lis[n];
    lis[0] = 1;
    //bottom up
    for (int i = 1; i < n; i++) {
        lis[i] = 1;
        for (int j = 0; j < i; j++)
            if (arr[i] > arr[j] and lis[i] < lis[j] + 1)
                lis[i] = lis[j] + 1;
    }
    //handy algorithm from <algorithm>:
    return *max_element(lis, lis + n);
}
int main(){
    int n;
    cin >> n;
    int arr[n];
    for(int i = 0; i < n; i++){
        cin >> arr[i];
    }
    cout << "Length of Longest increasing subsequence: " << LIS(arr,n) << endl;
}