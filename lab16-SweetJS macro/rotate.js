syntax rotate = function (ctx) {
    var inCtx = ctx.contextify(ctx.next().value);
    var result = #``;
    var flag = 0;
    var prev;
    var temp;
    result = result.concat(#`var temp;`); 
        for (let stx of inCtx) {
            if (flag == 0){
                temp = stx
                result = result.concat(#`temp = ${stx};`);
                flag = 1;
            }
            else{
                result = result.concat(#`${prev} = ${stx};`);
            }
            prev = stx
            inCtx.next(); // Eating comma
        }
    return result.concat(#`${prev} = temp;`);
}
var a = 1; var b = 2; var c = 3; var d = 4; var e = 5;
console.log("a: "+a+"b: "+b+"c: "+c+"d: "+d+"e: "+e);
rotate(a, b, c, d, e);
console.log("a: "+a+"b: "+b+"c: "+c+"d: "+d+"e: "+e);
rotate(a, b, c, d, e);
console.log("a: "+a+"b: "+b+"c: "+c+"d: "+d+"e: "+e);