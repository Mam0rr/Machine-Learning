import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestRegressor, GradientBoostingRegressor, StackingRegressor
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error, mean_absolute_error, r2_score
import numpy as np

def load_and_clean_data(file_path, target_column='mean_temp'):
    # Load data from CSV
    data = pd.read_csv(file_path)
    # Drop rows with NaN values
    data_cleaned = data.dropna()
    # Separate features and target
    X = data_cleaned.drop(columns=[target_column])
    y = data_cleaned[target_column]
    return train_test_split(X, y, test_size=0.2, random_state=42)

def evaluate_model(model, X_train, y_train, X_test, y_test):
    model.fit(X_train, y_train)
    y_pred = model.predict(X_test)
    mse = mean_squared_error(y_test, y_pred)
    mae = mean_absolute_error(y_test, y_pred)
    r2 = r2_score(y_test, y_pred)  # Calculate R^2 score
    return mse, mae, r2

def compare_models(file_path, target_column='mean_temp'):
    X_train, X_test, y_train, y_test = load_and_clean_data(file_path, target_column)
    
    # Initialize base models
    models = {
        'Random Forest': RandomForestRegressor(n_estimators=100, random_state=42),
        'Gradient Boosting': GradientBoostingRegressor(n_estimators=100, random_state=42),
        'Linear Regression': LinearRegression()
    }
    
    results = {}
    
    # Evaluate each base model
    for name, model in models.items():
        mse, mae, r2 = evaluate_model(model, X_train, y_train, X_test, y_test)
        results[name] = {'MSE': mse, 'MAE': mae, 'R^2': r2}
        print(f"{name}:")
        print(f"  Mean Squared Error: {mse:.2f}")
        print(f"  Mean Absolute Error: {mae:.2f}")
        print(f"  R^2 Score: {r2:.2f}\n")

    # Create a stacking model
    stacking_model = StackingRegressor(
        estimators=[
            ('rf', RandomForestRegressor(n_estimators=100, random_state=42)),
            ('gb', GradientBoostingRegressor(n_estimators=100, random_state=42)),
            ('lr', LinearRegression())
        ],
        final_estimator=LinearRegression()
    )
    
    # Evaluate the stacking model
    mse_stacking, mae_stacking, r2_stacking = evaluate_model(stacking_model, X_train, y_train, X_test, y_test)
    results['Stacking Model'] = {'MSE': mse_stacking, 'MAE': mae_stacking, 'R^2': r2_stacking}
    
    print("Stacking Model:")
    print(f"  Mean Squared Error: {mse_stacking:.2f}")
    print(f"  Mean Absolute Error: {mae_stacking:.2f}")
    print(f"  R^2 Score: {r2_stacking:.2f}\n")
    
    # Compare which model performed best based on MSE
    best_model = min(results, key=lambda x: results[x]['MSE'])
    print("Comparison Summary:")
    print(f"The model with the lowest Mean Squared Error is: {best_model} with MSE: {results[best_model]['MSE']:.2f}")

# Example usage
file_path = FILEPATHHERE
compare_models(file_path, target_column='mean_temp')
