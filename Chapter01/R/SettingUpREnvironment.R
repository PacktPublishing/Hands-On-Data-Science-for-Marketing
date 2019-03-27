# Test Data
data <- data.frame(
  "X"=c(0, 0.25, 0.5, 1), 
  "Y"=c(0, 0.5, 0.5, 1), 
  "output"=c(0, 0, 1, 1)
)

# Train Logistic Regression
logit.fit <- glm(
  output ~ X + Y, 
  data = data, 
  family = binomial
)

# Show Fitted Results
summary(logit.fit)

# Predict Class Probabilities
logit.probs <- predict(
  logit.fit, 
  newdata=data, 
  type="response"
)

# Predict Classes
logit.pred <- ifelse(logit.probs > 0.5, 1, 0)
logit.pred


# Plotting Library
library(ggplot2)

# Simple Scatterplot
ggplot(data, aes(x=X, y=Y, color=output)) +
  geom_point(size=3, shape=19) +
  ggtitle('Actual') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data, aes(x=X, y=Y, color=logit.pred)) + 
  geom_point(size=3, shape=19) +
  ggtitle('Predicted') +
  theme(plot.title = element_text(hjust = 0.5))
