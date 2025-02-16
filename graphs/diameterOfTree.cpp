#include<bits/stdc++.h>
#define pb push_back
using namespace std;
vector<vector<int> > adj;

void bfs(int u, map<int,int> dist,map<int, bool> visited){
    queue<int> q;
    q.push(u);
    while(!q.empty()){
        int fr = q.front();
        q.pop();
        vector<int>::iterator itr;
        for(itr = adj[fr].begin(); itr != adj[fr].end(); ++itr){
            if(!visited[*itr]){
                dist[*itr] = dist[fr] + 1;
                q.push(*itr);
                visited[*itr] = true;
            }
        }
    }
}
void dfs(int u,int v){

}
int main(){
int vertices,edges;
cin >> vertices >> edges;
adj.resize(vertices);
for(int i = 0; i < edges; i++){
    int u,v;
    cin >> u >> v;
    adj[u].pb(v);
    adj[v].pb(u);
    }
    int srcA;
    srcA = adj[0][0];
    map<int,int> distA;
    map<int,bool> visA;
    cout << "alive" << endl;
    bfs(srcA,distA,visA);
    cout << "Hello world" << endl;
    int maxA = 0;
    int srcB = -69;
    for(int i = 0; i < vertices; i++){
        if(distA[i] > maxA and i!=srcA){
            maxA = distA[i];
            srcB = i;
        }
    }
    cout << srcB << endl;
    /*map<int,int> distB;
    map<int,bool> visB;
    cout << "what happened " << endl;
    bfs(srcB,distB,visB);
    int maxB = 0;
    int srcC;
    cout << "am alive? " << endl;
    for(int i = 0; i < vertices; i++){
        if(distB[i] > maxB and i!=srcB){
            maxB = distB[i];
            srcC = i;
        }
    }
    cout << "here" << endl;
    cout << srcA << " " << srcB << " " << srcC << endl;*/
    //Diameter is from srcB to srcC. Let's print out that path, but first we must find it
    //We do this by using a simple DFS from srcB, and as soon as srcC pops up in the dfs, we end it. 
/*
7
6
0
1
0
2
2
6
0
3
3
4
3
5
*/
}