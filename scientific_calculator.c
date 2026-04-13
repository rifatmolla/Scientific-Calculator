#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <ctype.h>

#define PI 3.14159265358979323846
#define E  2.71828182845904523536

typedef enum { DEG, RAD } AngleMode;
AngleMode angle_mode = DEG;

static double memory = 0.0;
static double last_answer = 0.0;

double to_rad(double x) {
    return (angle_mode == DEG) ? x * PI / 180.0 : x;
}

double factorial(int n) {
    if (n < 0)  { printf("Error: factorial of negative number\n"); return NAN; }
    if (n > 20) { printf("Error: value too large (max 20!)\n"); return NAN; }
    double r = 1.0;
    for (int i = 2; i <= n; i++) r *= i;
    return r;
}

double combination(int n, int r) {
    if (r < 0 || r > n) return 0;
    return factorial(n) / (factorial(r) * factorial(n - r));
}

double permutation(int n, int r) {
    if (r < 0 || r > n) return 0;
    return factorial(n) / factorial(n - r);
}

void trim(char *s) {
    char *p = s;
    while (isspace((unsigned char)*p)) p++;
    memmove(s, p, strlen(p) + 1);
    int len = strlen(s);
    while (len > 0 && isspace((unsigned char)s[len - 1])) s[--len] = '\0';
}

int starts_with_ci(const char **sp, const char *prefix) {
    size_t len = strlen(prefix);
    if (strncasecmp(*sp, prefix, len) == 0) { *sp += len; return 1; }
    return 0;
}

double parse_expr(const char **sp);
double parse_term(const char **sp);
double parse_unary(const char **sp);
double parse_primary(const char **sp);

void skip_ws(const char **sp) {
    while (isspace((unsigned char)**sp)) (*sp)++;
}

double parse_expr(const char **sp) {
    double val = parse_term(sp);
    skip_ws(sp);
    while (**sp == '+' || **sp == '-') {
        char op = *(*sp)++;
        double rhs = parse_term(sp);
        val = (op == '+') ? val + rhs : val - rhs;
        skip_ws(sp);
    }
    return val;
}

double parse_term(const char **sp) {
    double val = parse_unary(sp);
    skip_ws(sp);
    while (**sp == '*' || **sp == '/' || **sp == '%') {
        char op = *(*sp)++;
        double rhs = parse_unary(sp);
        if (op == '*') val *= rhs;
        else if (op == '/') {
            if (rhs == 0.0) { printf("Error: division by zero\n"); return NAN; }
            val /= rhs;
        } else {
            val = fmod(val, rhs);
        }
        skip_ws(sp);
    }
    return val;
}

double parse_unary(const char **sp) {
    skip_ws(sp);
    if (**sp == '-') { (*sp)++; return -parse_unary(sp); }
    if (**sp == '+') { (*sp)++; return  parse_unary(sp); }
    double base = parse_primary(sp);
    skip_ws(sp);
    if (**sp == '^') {
        (*sp)++;
        double e = parse_unary(sp);
        base = pow(base, e);
    }
    return base;
}

double parse_primary(const char **sp) {
    skip_ws(sp);

    if (**sp == '(') {
        (*sp)++;
        double val = parse_expr(sp);
        skip_ws(sp);
        if (**sp == ')') (*sp)++;
        return val;
    }

    if (starts_with_ci(sp, "pi"))  return PI;
    if (starts_with_ci(sp, "e") && !isalpha((unsigned char)(*sp)[0])) return E;
    if (starts_with_ci(sp, "ans")) return last_answer;
    if (starts_with_ci(sp, "mr"))  return memory;

#define PARSE_2ARG(name, func)                   \
    if (starts_with_ci(sp, name)) {              \
        skip_ws(sp);                             \
        if (**sp == '(') (*sp)++;                \
        double a = parse_expr(sp);               \
        skip_ws(sp);                             \
        if (**sp == ',') (*sp)++;                \
        double b = parse_expr(sp);               \
        skip_ws(sp);                             \
        if (**sp == ')') (*sp)++;                \
        return func((int)a, (int)b);             \
    }

    if (starts_with_ci(sp, "pow")) {
        skip_ws(sp); if (**sp == '(') (*sp)++;
        double a = parse_expr(sp);
        skip_ws(sp); if (**sp == ',') (*sp)++;
        double b = parse_expr(sp);
        skip_ws(sp); if (**sp == ')') (*sp)++;
        return pow(a, b);
    }

    PARSE_2ARG("ncr", combination)
    PARSE_2ARG("npr", permutation)

#define PARSE_1ARG(name, func)                   \
    if (starts_with_ci(sp, name)) {              \
        skip_ws(sp);                             \
        if (**sp == '(') (*sp)++;                \
        double a = parse_expr(sp);               \
        skip_ws(sp);                             \
        if (**sp == ')') (*sp)++;                \
        return func(a);                          \
    }

#define PARSE_TRIG(name, func)                   \
    if (starts_with_ci(sp, name)) {              \
        skip_ws(sp);                             \
        if (**sp == '(') (*sp)++;                \
        double a = parse_expr(sp);               \
        skip_ws(sp);                             \
        if (**sp == ')') (*sp)++;                \
        return func(to_rad(a));                  \
    }

    PARSE_TRIG("sin", sin)
    PARSE_TRIG("cos", cos)
    PARSE_TRIG("tan", tan)

    if (starts_with_ci(sp, "asin")) {
        skip_ws(sp); if (**sp == '(') (*sp)++;
        double a = parse_expr(sp); skip_ws(sp); if (**sp == ')') (*sp)++;
        double r = asin(a);
        return (angle_mode == DEG) ? r * 180.0 / PI : r;
    }
    if (starts_with_ci(sp, "acos")) {
        skip_ws(sp); if (**sp == '(') (*sp)++;
        double a = parse_expr(sp); skip_ws(sp); if (**sp == ')') (*sp)++;
        double r = acos(a);
        return (angle_mode == DEG) ? r * 180.0 / PI : r;
    }
    if (starts_with_ci(sp, "atan")) {
        skip_ws(sp); if (**sp == '(') (*sp)++;
        double a = parse_expr(sp); skip_ws(sp); if (**sp == ')') (*sp)++;
        double r = atan(a);
        return (angle_mode == DEG) ? r * 180.0 / PI : r;
    }

    PARSE_1ARG("sinh",  sinh)
    PARSE_1ARG("cosh",  cosh)
    PARSE_1ARG("tanh",  tanh)
    PARSE_1ARG("log10", log10)
    PARSE_1ARG("log2",  log2)
    PARSE_1ARG("log",   log)
    PARSE_1ARG("exp",   exp)
    PARSE_1ARG("sqrt",  sqrt)
    PARSE_1ARG("cbrt",  cbrt)
    PARSE_1ARG("abs",   fabs)
    PARSE_1ARG("ceil",  ceil)
    PARSE_1ARG("floor", floor)
    PARSE_1ARG("round", round)

    if (starts_with_ci(sp, "fact")) {
        skip_ws(sp); if (**sp == '(') (*sp)++;
        double a = parse_expr(sp); skip_ws(sp); if (**sp == ')') (*sp)++;
        return factorial((int)a);
    }

    char *end;
    double val = strtod(*sp, &end);
    if (end == *sp) { printf("Error: unknown token '%c'\n", **sp); return NAN; }
    *sp = end;
    return val;
}

int handle_command(char *line) {
    trim(line);
    if (strcasecmp(line, "quit") == 0 || strcasecmp(line, "exit") == 0 || strcasecmp(line, "q") == 0) {
        printf("Bye.\n");
        exit(0);
    }
    if (strcasecmp(line, "mode deg") == 0) { angle_mode = DEG; printf("Mode: degrees\n"); return 1; }
    if (strcasecmp(line, "mode rad") == 0) { angle_mode = RAD; printf("Mode: radians\n"); return 1; }
    if (strcasecmp(line, "mc") == 0) { memory = 0.0; printf("Memory cleared\n"); return 1; }
    if (strcasecmp(line, "mr") == 0) { printf("Memory: %.10g\n", memory); return 1; }
    if (strcasecmp(line, "ms") == 0) { memory = last_answer; printf("Stored: %.10g\n", memory); return 1; }
    if (strcasecmp(line, "m+") == 0) { memory += last_answer; printf("Memory: %.10g\n", memory); return 1; }
    return 0;
}

int main(void) {
    char line[512];

    while (1) {
        printf("> ");
        if (!fgets(line, sizeof(line), stdin)) break;
        line[strcspn(line, "\n")] = '\0';
        if (strlen(line) == 0) continue;
        if (handle_command(line)) continue;

        const char *p = line;
        double result = parse_expr(&p);
        skip_ws(&p);

        if (*p != '\0') { printf("Error: unexpected input near '%s'\n", p); continue; }
        if (isnan(result))  { printf("Error: not a number\n"); continue; }
        if (isinf(result))  { printf("Error: infinity\n"); continue; }

        last_answer = result;
        printf("= %.10g\n", result);
    }

    return 0;
}
