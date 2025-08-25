\begin{table}[h!]
\centering
\caption{Vergleich der besten Modelle (Test-Set).}
\begin{tabular}{lcccc}
\hline
\textbf{Modell} & \textbf{Accuracy} & \textbf{AUC} & \textbf{Log Loss} & \textbf{Brier Score} \\
\hline
eXtreme Gradient Boosting (Tuned) & 0.625 & 0.713 & 0.620  & 0.216 \\
Logistische Regression            & 0.652 & 0.712 & 0.619  & 0.216 \\
Random Forest (Tuned)             & 0.650 & 0.705 & 0.746  & 0.277 \\
Random Forest                     & 0.651 & 0.705 & 0.970  & 0.263 \\
Baseline (Rankpunkte)             & 0.611 & 0.703 & 0.637  & 0.222 \\
eXtreme Gradient Boosting         & 0.637 & 0.692 & 0.643  & 0.224 \\
Einfache Baseline (immer Favorit) & 0.648 & 0.648 & 12.142 & 0.352 \\
\hline
\end{tabular}
\end{table}
