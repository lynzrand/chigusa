int test(int a){
    return a+1;
}
int add(int a, int b){
    return a+b;
}
void main(){
    int a=3;
    print(a);
    print(test(a));
    print(add(test(a),a));
}
