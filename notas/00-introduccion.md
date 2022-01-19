


# Notación

Usaremos la convención usual en probabilidad. 


## Variables aleatorias

Una variable aleatoria \(X\) está definida en un `espacio de probabilidad` \((\mathcal{X}, \mathcal{F}, \pi)\). La función \(\pi: \mathcal{F}\rightarrow[0,1]\)  se llama la `función de distribución` de la variable aleatoria \(X\). Escribimos \(X \sim \pi\).


## Distribución paramétrica

Decimos que una función de distribución es `paramétrica` si se puede identificar completamente la distribución con respecto a un `vector de parámetros` \(\theta \in \mathbb{R}^p\). Esto lo denotamos de la siguiente manera

\begin{align}
\pi_\theta(x) \qquad \text{} \pi(x ; \theta)\,,
\end{align}

y si  \(\theta \neq\theta'\) entonces \(\pi_\theta(x) \neq \pi_{\theta'}(x)\) para cualquier \(x\) en el soporte.


## Valores esperados

El `valor esperado` de una variable aleatoria \(X \sim \pi\) se define como

\begin{align}
\mathbb{E}[X] = \int_{\mathcal{X}} x \, \pi(x) \text{d}x\,.
\end{align}


### Abuso de notación

La `función de densidad` está definida como \(\text{d}\pi/\text{d}x\). En la definición de valor esperado deberíamos de haber escrito \(\text{d}\pi(x)\)  o bien \(\pi(\text{d}x)\) (integrales de Lebesgue). Pero para no ofuscar notación lo obviamos&#x2026;


## Estadísticas de interés

La definición se puede extender con \(f: \mathcal{X} \rightarrow \mathbb{R}\) y se calcula como

\begin{align}
\mathbb{E}[f(X)] = \int_{\mathcal{X}} f(x) \pi(x) \text{d}x\,.
\end{align}

Denotaremos de la siguiente manera

\begin{align}
\pi(f) := \mathbb{E}[f(X)]\,.
\end{align}


## Probabilidad condicional

La `probabilidad condicional` de \(A\) dado el evento \(B\) se denota \(\pi(A|B)\) y está definida como

\begin{align}
\pi(A|B) = \frac{\pi(A \cap B)}{\pi(B)}
\end{align}


## Regla de Bayes

La `regla de Bayes` utiliza la definición de probabilidad condicional para hacer inferencia a través de 

\begin{align}
\pi(A|B) = \frac{\pi(B|A) \pi(A)}{\pi(B)}\,.
\end{align}


## Ejemplos

-   Verosimilitud: \(x |\theta \sim \mathsf{Binomial}(n, \theta)\) + Previa: \(\theta \sim \mathsf{Beta}(\alpha, \beta)\) = Posterior: ?
-   Verosimilitud: \(x |\theta \sim \mathsf{Uniforme}(0, \theta)\) + Previa: \(\theta \sim \mathsf{Pareto}(\theta_0)\) = Posterior: ?


# Bibliografia

 
<references.bib>

