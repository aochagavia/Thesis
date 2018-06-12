(* http://coq-blog.clarus.me/write-a-script-in-coq.html *)

Add LoadPath ".".

Load CpdtTactics.
Require Import Coq.Bool.Bool.
Require Import Coq.Lists.List.
Require Import Coq.Program.Basics.
Import ListNotations.

Fixpoint intercalate {T : Type} (x : list T) (xs : list (list T)) : list T :=
  match xs with
  | [] => []
  | [y] => y
  | y :: ys => y ++ x ++ intercalate x ys
  end.

Definition concatMap {T U : Type} (f : T -> list U) (xs : list T) : list U :=
  concat (map f xs).

Fixpoint replicate {T} n (x : T) : list T :=
  match n with
  | 0 => []
  | S n' => x :: replicate n' x
  end.

Fixpoint mapMaybe {T U} (f : T -> option U) (xs : list T) : list U :=
  match xs with
  | [] => []
  | y :: ys => match f y with
               | Some y' => y' :: mapMaybe f ys
               | None => mapMaybe f ys
               end
  end.

Fixpoint and (xs : list bool) : bool :=
  match xs with
  | [] => true
  | false :: _ => false
  | true :: ys => and ys
  end.

Definition fmap {T U} (f : T -> U) (x : option T) : option U :=
  match x with
  | Some y => Some (f y)
  | None => None
  end.

Definition isNothing {T} (x : option T) : bool :=
  match x with
  | Some _ => false
  | None => true
  end.

Definition isJust {T} (x : option T) : bool :=
  match x with
  | Some _ => true
  | None => false
  end.

Definition maybeToList {T} (x : option T) : list T :=
  match x with
  | Some y => [y]
  | None => []
  end.

(* Fixpoint heads {T} (xss : list (list T)) : list T :=
  match xss with
  | [] => []
  | [] :: yss => heads yss
  | (y :: _) :: yss => y :: heads yss
  end.

Fixpoint tails {T} (xss : list (list T)) : list (list T) :=
  match xss with
  | [] => []
  | [] :: yss => tails yss
  | (_ :: ys) :: yss => ys :: tails yss
  end.

Fixpoint transpose {T} (xss : list (list T)) {struct xss} : list (list T) :=
  match xss with
  | [] => []
  | [] :: yss => transpose yss
  | (y :: ys) :: yss => (y :: heads yss) :: transpose yss
  end. *)

(* transpose               :: [[a]] -> [[a]]
transpose []             = []
transpose ([]   : xss)   = transpose xss
transpose ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : transpose (xs : [ t | (_:t) <- xss]) *)

(* CoFixpoint cycle' {T} (xs : list T) (used : list T) : Streams.Stream T :=
  match xs with
  | [] => cycle' used xs
  | y :: ys => Streams.Cons y (cycle' ys (used ++ [y]))
  end.

CoFixpoint cycle {T} (x : list T) : Streams.Stream T :=
  Streams.Cons x (cycle x).

Fixpoint take {T} n (xs : Streams.Stream T) : list T :=
  match n with
  | 0 => []
  | S n' => match xs with
            | Streams.Cons y ys => y :: take n' ys
            end
  end. *)

Lemma id_works : forall (T : Type) (x : T), id x = x.
  Proof. auto. Qed.

Lemma cong_cons : forall {T} (x : T) xs ys, xs = ys <-> x :: xs = x :: ys.
Proof. intros. split. congruence. congruence. Qed.

Definition bad_style_map {T U} (f : T -> U) (xs : list T) : list U :=
  match xs with
  | [] => []
  | ys => map f ys
  end.

Theorem empty_base_case : forall {T U} (f : T -> U) xs, bad_style_map f xs = map f xs.
  Proof. intros. destruct xs. auto. auto. Qed.

Theorem singleton_concat : forall {T : Type} (x : T) xs, [x] ++ xs = x :: xs.
  Proof. auto. Qed.
Theorem empty_append_1 : forall {T : Type} (xs : list T), [] ++ xs = xs.
  Proof. auto. Qed.
Theorem empty_append_2 : forall {T : Type} (xs : list T), xs ++ [] = xs.
  Proof. intuition. Qed.

(* intros. rewrite app_nil_r. reflexivity. *)

Theorem partial_map : forall {T U : Type} (f : T -> U) (x : T) xs,
    map f (x :: xs) = f x :: map f xs.
  Proof. auto. Qed.
Theorem partial_concat : forall {T : Type} (x : list T) xs, concat (x :: xs) = x ++ concat xs.
  Proof. auto. Qed.
Theorem empty_concat : forall {T : Type}, @concat T [] = [].
  Proof. auto. Qed.

(* Theorem take_cycle : forall T n (e : T), take n (cycle [e]) = replicate n e. *)

Theorem intercalate_concat : forall {T: Type} (xs : list (list T)), intercalate [] xs = concat xs.
  Proof. intros. induction xs.
    auto.
    simpl. rewrite IHxs. induction xs.
      unfold concat. rewrite app_nil_r. reflexivity.
      reflexivity.
  Qed.

Theorem foldr_append : forall {T : Type} (xs : list (list T)),
    @fold_right (list T) (list T) (@app T) [] xs = concat xs.
  Proof. auto. Qed.

Theorem foldr_cons : forall {T : Type} (xs ys : list T),
    fold_right cons xs ys = ys ++ xs.
  Proof. intros. induction ys.
    auto.
    simpl. congruence.
  Qed.

Theorem concat_map : forall {T U : Type} (f : T -> list U) xs,
    concat (map f xs) = concatMap f xs.
  Proof. auto. Qed.

Theorem concat_map_filter : forall {T : Type} (cond : T -> bool) (xs : list T), concatMap (fun x => if cond x then [x] else []) xs = filter cond xs.
  Proof. intros. induction xs.
    auto.
    simpl. rewrite <- IHxs. unfold concatMap. simpl. destruct (cond a).
      auto.
      auto.
  Qed.

Theorem zero_index : forall T (xs : list T), nth_error xs 0 = hd_error xs.
  Proof. auto. Qed.

Theorem map_id : forall (T : Type) (xs : list T), map id xs = id xs.
  Proof.
    intros T xs.
    induction xs.
      auto.
      simpl. rewrite id_works. rewrite id_works. apply (cong_cons a (map id xs) xs). auto.
  Qed.

Theorem foldr_empty : forall {A B : Type} (f : B -> A -> A) (acc : A),
    fold_right f acc [] = acc.
  Proof. auto. Qed.

Theorem concat_map_to_map : forall {T} (xs : list T), concatMap (fun x => [x]) xs = map (fun x => x) xs.
  Proof. intros. induction xs.
    auto.
    simpl.
      rewrite <- (cong_cons a (concat (map (fun x => [x]) xs)) (map (fun x => x) xs)).
      rewrite <- IHxs.
      rewrite (concat_map _ _).
      reflexivity.
  Qed.

Theorem concat_map_comp : forall {A B C} (f : B -> list C) (g : A -> B) (xs : list A), concatMap f (map g xs) = concatMap (compose f g) xs.
  Proof. intros. induction xs.
    auto.
    simpl. unfold compose. fold (compose f g).
      rewrite <- (concat_map f _). rewrite <- (concat_map _ (a :: xs)). simpl.
      rewrite concat_map. rewrite concat_map. rewrite IHxs. reflexivity.
  Qed.

Theorem concat_map_flip : forall T (xs : list T), concatMap (flip cons []) xs = id xs.
  Proof. intros. induction xs.
    auto.
    unfold id in IHxs. simpl. unfold id. apply cong_cons. auto.
  Qed.

Theorem concat_replicate : forall {T} n (x : T), concat (replicate n [x]) = replicate n x.
  Proof. intros. induction n.
    auto.
    simpl. congruence.
  Qed.

Theorem map_map_comp : forall {A B C} (f : B -> C) (g : A -> B) (xs : list A), map f (map g xs) = map (compose f g) xs.
  Proof. intros. induction xs.
    auto.
    simpl. rewrite IHxs. unfold compose. reflexivity.
  Qed.

Theorem map_maybe_fmap : forall {A B} (f : nat -> B) (xs : list A), mapMaybe (fun x => fmap f (Some 42)) xs = map f (mapMaybe (fun x => Some 42) xs).
  Proof. intros. induction xs.
    auto.
    simpl. rewrite <- cong_cons. auto.
  Qed.

Theorem map_maybe_just : forall T (xs : list T), mapMaybe (fun x => Some 42) xs = map (fun x => 42) xs.
  Proof. intros. induction xs.
    auto.
    simpl. congruence.
  Qed.

Theorem cons_append : forall T (x : T) (xs ys : list T), (x :: xs) ++ ys = x :: (xs ++ ys).
  Proof. auto. Qed.

Theorem append_append : forall T (xs ys zs : list T), (xs ++ ys) ++ zs = xs ++ (ys ++ zs).
  Proof. crush. Qed.

Theorem is_nothing : forall T (x : option T), x = None <-> isNothing x = true.
  Proof. intros. unfold isNothing. destruct x. unfold iff.
    split. discriminate. discriminate.
    split. auto. auto.
  Qed.

Theorem is_just : forall T y (x : option T), x = Some y -> isJust x = true.
  Proof. crush. Qed.

Theorem maybe_to_list_1 : forall T (x : option T), maybeToList x = [] <-> isNothing x = true.
  Proof. intros. destruct x.
    simpl. split. discriminate. intros. contradict H. auto. 
    simpl. split. auto. auto.
  Qed.

Theorem maybe_to_list_2 : forall T (x : option T), maybeToList x <> [] <-> isJust x = true.
  Proof. intros. destruct x.
    simpl. split. auto. intros. pose proof nil_cons as P. auto.
    simpl. split. auto. intros. contradict H. auto.
  Qed.

(* Note: as far as I know all functions in Coq must be total, therefore there is no way to define fromJust *)
Theorem maybe_to_list_3 : forall T (x : option T), nth_error (maybeToList x) 0 = x.
  Proof. intros. destruct x. auto. auto. Qed.

Theorem not_is_just : forall T (x : option T), isNothing x = negb (isJust x).
  Proof. intros. destruct x. auto. auto. Qed.

Theorem true_id : forall b, eqb b true = id b.
  Proof. destr_bool. Qed.

Theorem false_neg : forall b, eqb b false = negb b.
  Proof. destr_bool. Qed.

Theorem and_id : forall b, andb b true = id b.
  Proof. destr_bool. Qed.

Theorem foldr_and : forall xs, fold_right andb true xs = and xs.
  Proof. auto. Qed.

Theorem and_map : forall T (f : T -> bool) xs, and (map f xs) = forallb f xs.
  Proof. intros. induction xs.
    auto.
    simpl. unfold andb. rewrite IHxs. reflexivity.
  Qed.

Theorem all_id : forall xs, forallb id xs = and xs.
  Proof. crush. Qed.

Theorem simplify_if : forall (cond : bool), (if cond then true else false) = cond.
  Proof. destr_bool. Qed.

Theorem switch_if_branches : forall T cond (x y : T), (if negb cond then x else y) = (if cond then y else x).
  Proof. destr_bool. Qed.
