import numpy as np
import random
import matplotlib.pyplot as plt
from scipy.stats import pearsonr
import colorednoise as cn

np.random.seed(42)
random.seed(42)

def wykres(x):
    m, n = x.shape
    for i in range(m):
        plt.subplot(m, 1, i + 1)
        plt.plot(x[i, :])
    plt.show()

def generator(k):
    t = np.arange(1, k + 1)
    s1 = np.random.randn(1, k)
    s2 = np.sin(2 * np.pi / 100 * t)
    s3 = np.sign(np.sin(2 * np.pi / 50 * t))
    s4 = np.sin(2 * np.pi * t / 150) + np.cos(2 * np.pi * t / 80) * np.exp(-0.01 * t)
    s5 = 2 * np.abs((t / 50) % 1 - 0.5) - 1
    s6 = cn.powerlaw_psd_gaussian(1, k, random_state= np.random.default_rng(seed=42))
    s = np.vstack([s1, s2, s3, s4, s5, s6])
    a = np.random.randn(s.shape[0], s.shape[0])
    x = np.dot(a, s)
    # no need for condition matrix (c = np.linalg.cond(a)), we are tackling every problem
    return s, x, a

def xx3(y,i):
    f = y[:, i] ** 3
    g = y[:, i]
    return f,g

def tanh(y,i):
    f = np.tanh(y[:, i])
    g = 1 - np.tanh(y[:, i]) ** 2
    return f,g

def exp(y,i):
    f = np.exp(-y[:, i] ** 2)
    g = -2 * y[:, i] * np.exp(-y[:, i] ** 2)
    return f, g

def LReLU(y,i):
    f = np.maximum(0.01 * y[:, i], y[:, i])
    g = np.where(y[:, i] > 0, 1, 0.01)
    return f,g

def sigm(y,i):
    f = 1 / (1 + np.exp(-y[:, i]))
    g = f * (1 - f)
    return f,g

# ICA function
def icaalg(x, c1, c2, c3,z):
    m, n = x.shape
    if m > n:
        x = x.T
    n, k = x.shape
    w = np.eye(n)
    y = np.zeros((n, k))

    for i in range(k):
        if i < c1:
            ei = c2
        else:
            ei = c2 * np.exp(c3 * (i - c1))

        y[:, i] = np.dot(w, x[:, i])
        # Place to change non-linearity underneath
        list_of_nl = [xx3(y,i), tanh(y,i), exp(y,i), LReLU(y,i), sigm(y,i)]
        f,g = list_of_nl[z]

        w += ei * (np.eye(n) - np.outer(f, g)) @ w
    return w


# Evaluation metric (Sum of Squared Differences)
def evaluate(w, a):
    w_inv = np.linalg.pinv(w)
    diff = a - w_inv
    return np.sum(diff ** 2)

# Evaluation metric (Pearson's correlation)
def evaluate_signal_correlation(w, a, s, x):

    estimated_s = np.dot(w, x)

    def normalize(signal):
        return (signal - np.mean(signal)) / np.std(signal)

    scores = []
    for i in range(s.shape[0]):
        estimated = normalize(estimated_s[i])
        original = normalize(s[i])
        corr, _ = pearsonr(estimated, original)
        scores.append(abs(corr))

    return -np.mean(scores)


# Simulated Annealing 1st approach
def simulated_annealing(x, a, s, c1_range, c2_range, c3_range, k, initial_temp=10.0, cooling_rate=0.95, max_iter=100):
    c1 = random.uniform(*c1_range)
    c2 = random.uniform(*c2_range)
    c3 = random.uniform(*c3_range)

    current_temp = initial_temp
    best_params = (c1, c2, c3)
    best_score = evaluate_signal_correlation(icaalg(x, c1, c2, c3, k), a, s, x)

    for iteration in range(max_iter):
        delta_c1 = random.uniform(-0.1, 0.1) * current_temp
        delta_c2 = random.uniform(-0.01, 0.01) * current_temp
        delta_c3 = random.uniform(-0.001, 0.001) * current_temp

        c1_new = np.clip(best_params[0] + delta_c1, *c1_range)
        c2_new = np.clip(best_params[1] + delta_c2, *c2_range)
        c3_new = np.clip(best_params[2] + delta_c3, *c3_range)

        w = icaalg(x, c1_new, c2_new, c3_new,k)
        score = evaluate_signal_correlation(w, a, s, x)

        if score < best_score or random.random() < np.exp(-(score - best_score) / current_temp):
            best_params = (c1_new, c2_new, c3_new)
            best_score = score

        current_temp *= cooling_rate

    return best_params, best_score

# Simulated Annealing 2nd approach - temperature influences the range of exploration
def simulated_annealing_local(x, a, s, c1_range, c2_range, c3_range, k, initial_temp=10.0, cooling_rate=0.95,
                              max_iter=100):
    c1 = random.uniform(*c1_range)
    c2 = random.uniform(*c2_range)
    c3 = random.uniform(*c3_range)

    current_temp = initial_temp
    best_params = (c1, c2, c3)
    best_score = evaluate_signal_correlation(icaalg(x, c1, c2, c3, k), a,s,x)

    for iteration in range(max_iter):
        delta_c1 = (c1_range[1] - c1_range[0]) * current_temp / initial_temp
        delta_c2 = (c2_range[1] - c2_range[0]) * current_temp / initial_temp
        delta_c3 = (c3_range[1] - c3_range[0]) * current_temp / initial_temp

        c1_new = np.clip(c1 + random.uniform(-delta_c1, delta_c1), *c1_range)
        c2_new = np.clip(c2 + random.uniform(-delta_c2, delta_c2), *c2_range)
        c3_new = np.clip(c3 + random.uniform(-delta_c3, delta_c3), *c3_range)

        w = icaalg(x, c1_new, c2_new, c3_new,k)
        score = evaluate_signal_correlation(w, a, s, x)

        if score < best_score or random.random() < np.exp(-(score - best_score) / current_temp):
            c1, c2, c3 = c1_new, c2_new, c3_new
            best_score = score
            best_params = (c1, c2, c3)

        current_temp *= cooling_rate

    return best_params, best_score


# Random Search with Local Exploitation
def random_search_with_local(x, a, s, c1_range, c2_range, c3_range, k, n_samples=50, n_exploit=10):
    best_params = None
    best_score = 999
    sampled_params = []
    for _ in range(n_samples):
        c1 = random.uniform(*c1_range)
        c2 = random.uniform(*c2_range)
        c3 = random.uniform(*c3_range)
        w = icaalg(x, c1, c2, c3,k)
        score = evaluate_signal_correlation(w, a, s, x)
        sampled_params.append((score, (c1, c2, c3)))
        if score < best_score:
            best_score = score
            best_params = (c1, c2, c3)
    sampled_params.sort(key=lambda x: x[0])
    for _, params in sampled_params[:n_exploit]:
        c1, c2, c3 = params
        for delta_c1 in np.linspace(-0.1, 0.1, 5):
            for delta_c2 in np.linspace(-0.01, 0.01, 5):
                for delta_c3 in np.linspace(-0.001, 0.001, 5):
                    c1_new = c1 + delta_c1
                    c2_new = c2 + delta_c2
                    c3_new = c3 + delta_c3
                    w = icaalg(x, c1_new, c2_new, c3_new, k)
                    score = evaluate_signal_correlation(w, a, s, x)

                    if score < best_score:
                        best_score = score
                        best_params = (c1_new, c2_new, c3_new)
    return best_params, best_score


k = 1000
s, x, a = generator(k)
wykres(s)
c1_range = (50, 200)
c2_range = (0.001, 0.01)
c3_range = (-0.01, 0.0)




# Testing wether cross-validation makes sense (apparently it does, as long as you know the type of signal you have to
# handle beforehand)
def tes_cross_validation(iterations=100, n_samples=1000):
    params_default = (150, 0.008, -0.001)
    params_previous = (65.3299851920895, 0.003105388410568135, -0.00017976324221633097)

    default_scores = []
    previous_scores = []
    optimized_scores = []

    for _ in range(iterations):
        try:
            s,x,a = generator(n_samples)

            w_default = icaalg(x, *params_default, 0)
            default_score = evaluate_signal_correlation(w_default, a, s, x)
            if not np.isfinite(default_score):
                raise ValueError("Default score is not finite.")
            default_scores.append(default_score)

            w_previous = icaalg(x, *params_previous, 0)
            previous_score = evaluate_signal_correlation(w_previous, a, s, x)
            if not np.isfinite(previous_score):
                raise ValueError("Previous score is not finite.")
            previous_scores.append(previous_score)

            best_params = simulated_annealing_local(x, a, s, (50, 200), (0.001, 0.01), (-0.01, 0), 0)[0]
            w_optimized = icaalg(x, *best_params, 0)
            optimized_score = evaluate_signal_correlation(w_optimized, a, s, x)
            if not np.isfinite(optimized_score):
                raise ValueError("Optimized score is not finite.")
            optimized_scores.append(optimized_score)

        except (ValueError, RuntimeWarning) as e:
            print(f"Warning: {e} - Skipping this iteration.")
            continue

        print(f"  Default Score: {default_score}")
        print(f"  Previous Score: {previous_score}")
        print(f"  Optimized Score: {optimized_score}")

    print("Default Params Mean Score:", np.mean(default_scores) if default_scores else "No valid results")
    print("Previous Params Mean Score:", np.mean(previous_scores) if previous_scores else "No valid results")
    print("Optimized Params Mean Score:", np.mean(optimized_scores) if optimized_scores else "No valid results")

#tes_cross_validation()

results = []
b_results = [{}]
b_score = 0
for z in range(5):
    for method in ["simulated_annealing", "random_search_with_local"]:
        try:
            if method == "simulated_annealing":
                best_params, best_score = simulated_annealing_local(
                    x, a, s, c1_range, c2_range, c3_range, z, initial_temp=10.0, cooling_rate=0.95, max_iter=100
                )
            elif method == "random_search_with_local":
                best_params, best_score = random_search_with_local(
                    x, a, s, c1_range, c2_range, c3_range, z, n_samples=50, n_exploit=10
                )
            else:
                raise ValueError("Unknown optimization method")
            # Zapis wyniku
            if best_score < b_score:
                b_score = best_score
                b_results[0] = {
                    "nonlinearity": z,
                    "method": method,
                    "best_params": best_params,
                    "best_score": best_score
                }

            results.append({
                "nonlinearity": z,
                "method": method,
                "best_params": best_params,
                "best_score": best_score
            })
            print(f"Finished: Nonlinearity {z}, Method {method}, Best Score: {best_score}")

        except ValueError as e:
            print(f"Error for Nonlinearity {z}, Method {method}: {e}")

# Wyświetlenie wyników
print(b_results)
wykres(icaalg(x,*b_results[0]["best_params"],b_results[0]["nonlinearity"])@x)
for result in results:
    print(f"Nonlinearity: {result['nonlinearity']}, Method: {result['method']}, "
          f"Best Params: {result['best_params']}, Best Score: {result['best_score']}")