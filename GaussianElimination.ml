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