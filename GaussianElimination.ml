(* TODO *)
let solve (m : Matrix) (b : array) =
    let length = Array.length b in
    for i = 0 to length - 1 do
        let max = i in
        for j = i + 1 to length - 1 do
            if (abs m.(j).(i)) > (abs m.(max).(i)) then max = i
        done;

        let temp_row = m.(i) in
            m.(i) <- m.(max);
            m.(max) <- temp_row;
        let temp_b = b.(i) in
            b.(i) <- b.(max);
            b.(max) <- temp_b;

        for j = i + 1 to length - 1 do
            let factor = M.div m.(j).(i)) m.(i).(i) in
            b.(j) <- M.sub b.(j) (M.mul factor b.(i));
            for p = i to length - 1 do
                m.(j).(p) <- M.sub m.(j).(p) (M.mul factor m.(i).(p));
            done;
        done;    
    done; 

    let soluton = Array.make length 0 in
    for i = rows - 1 downto 0 do
        let sum = 0.0 in
        for j = i + 1 to rows - 1 do
            sum = M.add sum (M.mul a.(i).(j) solution.(j))
        done;
        solution.(i) <- M.div (M.sub b.(i) sum) m.(i).(i);
    done;
    (m, b)

(* JAVA CODE TEMPLATE
// Gaussian elimination with partial pivoting
    public static double[] lsolve(double[][] A, double[] b) {
        int N  = b.length;

        for (int p = 0; p < N; p++) {

            // find pivot row and swap
            int max = p;
            for (int i = p + 1; i < N; i++) {
                if (Math.abs(A[i][p]) > Math.abs(A[max][p])) {
                    max = i;
                }
            }
            double[] temp = A[p]; A[p] = A[max]; A[max] = temp;
            double   t    = b[p]; b[p] = b[max]; b[max] = t;

            // singular or nearly singular
            if (Math.abs(A[p][p]) <= EPSILON) {
                throw new RuntimeException("Matrix is singular or nearly singular");
            }

            // pivot within A and b
            for (int i = p + 1; i < N; i++) {
                double alpha = A[i][p] / A[p][p];
                b[i] -= alpha * b[p];
                for (int j = p; j < N; j++) {
                    A[i][j] -= alpha * A[p][j];
                }
            }
        }

        // back substitution
        double[] x = new double[N];
        for (int i = N - 1; i >= 0; i--) {
            double sum = 0.0;
            for (int j = i + 1; j < N; j++) {
                sum += A[i][j] * x[j];
            }
            x[i] = (b[i] - sum) / A[i][i];
        }
        return x;
    }
*)