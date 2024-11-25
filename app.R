if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("catR", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("catR", quietly = TRUE)) install.packages("ltm")
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
# Завантаження необхідних бібліотек
library(shiny)
library(readxl)
library(dplyr)
library(ltm)

# Завантаження даних із файлу Excel
file_path <- "C:/Users/HP/OneDrive/Документи/catProject/questions.xlsx"
questions_df <- read_excel(file_path) %>%
  mutate(
    Answer = trimws(Answer),  # Очищення пробілів у правильній відповіді
    correct_answer = trimws(correct_answer),  # Очищення пробілів у тексті правильної відповіді
    difficulty = as.numeric(difficulty)  # Перетворення складності на числовий тип
  )

# Функція ймовірності правильної відповіді за моделлю Раша
prob_correct <- function(theta, difficulty) {
  1 / (1 + exp(-(theta - difficulty)))
}

# Функція для розрахунку інформації Кульбака-Лейблера
information_kl <- function(theta, difficulty) {
  p <- prob_correct(theta, difficulty)
  return(p * (1 - p))
}

# Функція обчислення логарифмічної функції правдоподібності
log_likelihood <- function(theta, responses, difficulties) {
  likelihood <- 0
  for (i in 1:length(responses)) {
    p <- prob_correct(theta, difficulties[i])
    if (responses[i] == 1) {
      likelihood <- likelihood + log(p)
    } else {
      likelihood <- likelihood + log(1 - p)
    }
  }
  return(-likelihood)
}

# Функція для вибору наступного запитання за методом Кульбака-Лейблера
select_next_question <- function(questions, user_ability, answered_questions) {
  remaining_questions <- questions[!(questions$`SI No` %in% answered_questions), ]
  
  if (nrow(remaining_questions) == 0) {
    return(NULL)
  }
  
  remaining_questions$information <- sapply(remaining_questions$difficulty, function(d) {
    information_kl(user_ability, d)
  })
  
  next_question_index <- which.max(remaining_questions$information)
  return(remaining_questions$`SI No`[next_question_index])
}

# Інтерфейс Shiny
ui <- fluidPage(
  titlePanel("Адаптивний тест"),
  textOutput("question_text"),
  uiOutput("answer_choices"),  # Динамічний UI для варіантів відповіді
  actionButton("submit", "Відправити відповідь"),
  textOutput("feedback"),
  actionButton("next_question", "Далі"),  # Кнопка для переходу до наступного питання
  actionButton("end_test", "Завершити тестування"),  # Кнопка для завершення тестування
  textOutput("ability_estimate")
)


server <- function(input, output, session) {
  user_ability <- reactiveVal(0)
  answered_questions <- reactiveVal(integer(0))
  responses <- reactiveVal(integer(0))
  current_question <- reactiveVal(NULL)
  show_next_question <- reactiveVal(FALSE)  # Перемикач для відображення наступного питання
  submit_disabled <- reactiveVal(FALSE)  # Прапорець для блокування кнопки "Відправити відповідь"
  initial_questions_count <- 5  # Кількість запитань перед першою оцінкою
  test_ended <- reactiveVal(FALSE)  # Прапорець для завершення тестування
  
  # Ініціалізація перших запитань
  observe({
    if (is.null(current_question()) && length(answered_questions()) < initial_questions_count && !test_ended()) {
      # Випадковий вибір запитання
      next_question_index <- sample(1:nrow(questions_df), 1)
      current_question(questions_df[next_question_index, ])
      
      # Відображення обраного запитання
      output$question_text <- renderText({ current_question()$Question })
      submit_disabled(FALSE)  # Дозволяємо натискання кнопки "Відправити відповідь"
      
      # Оновлення варіантів відповіді
      output$answer_choices <- renderUI({
        question <- current_question()
        # Формування списку варіантів відповіді з текстом
        choices <- setNames(
          c("A", "B", "C", "D"),
          c(paste("A:", question$A),
            paste("B:", question$B),
            paste("C:", question$C),
            paste("D:", question$D))
        )
        radioButtons("user_response", "Виберіть відповідь:", choices = choices)
      })
      
      output$feedback <- renderText("")  # Очищення тексту пояснення
    }
  })
  
  observeEvent(input$end_test, {
    test_ended(TRUE)  # Встановлюємо прапорець завершення тесту
    output$question_text <- renderText("Тест завершено. Остаточна оцінка рівня знань: " 
                                       %>% paste(round(user_ability(), 2)))
    output$answer_choices <- renderUI(NULL)
    output$feedback <- renderText("")
    show_next_question(FALSE)
    stopApp()  # Зупинка роботи додатка Shiny
  })
  
  
  observeEvent(input$submit, {
    if (is.null(current_question()) || submit_disabled()) return()
    
    question <- current_question()
    # Витягнення літери відповіді (перший символ перед ":")
    user_response <- sub(":.*$", "", trimws(input$user_response))
    correct_answer <- trimws(question$Answer)  # Очищення пробілів у правильній відповіді
    
    # Порівняння відповідей
    is_correct <- ifelse(user_response == correct_answer, 1, 0)
    
    responses(c(responses(), is_correct))
    answered_questions(c(answered_questions(), question$`SI No`))
    
    # Виведення пояснення
    if (is_correct == 1) {
      output$feedback <- renderText("Правильно!")
    } else {
      output$feedback <- renderText(paste("Неправильно. Правильна відповідь", correct_answer, ": ",
                                          question$correct_answer))
    }
    
    # Оновлення оцінки рівня знань за допомогою MLE після перших кількох запитань
    if (length(answered_questions()) >= initial_questions_count) {
      result <- optimize(f = log_likelihood, interval = c(-3, 3),
                         responses = responses(), difficulties = questions_df$difficulty[answered_questions()])
      user_ability(result$minimum)
    }
    
    output$ability_estimate <- renderText(paste("Оновлена оцінка рівня знань:", round(user_ability(), 2)))
    
    # Заборона повторного натискання кнопки "Відправити відповідь"
    submit_disabled(TRUE)
    
    # Встановлення прапорця для відображення наступного питання
    show_next_question(TRUE)
  })
  
  observeEvent(input$next_question, {
    if (!show_next_question()) return()
    
    show_next_question(FALSE)  # Скидаємо прапорець
    
    # Вибір наступного запитання
    remaining_questions <- questions_df[!(questions_df$`SI No` %in% answered_questions()), ]
    if (nrow(remaining_questions) == 0) {
      current_question(NULL)
      output$question_text <- renderText("Усі запитання пройдено. Тест завершено.")
      output$answer_choices <- renderUI(NULL)
    } else {
      remaining_questions$information <- sapply(remaining_questions$difficulty, function(d) {
        information_kl(user_ability(), d)
      })
      next_question_index <- which.max(remaining_questions$information)
      current_question(remaining_questions[next_question_index, ])
      output$question_text <- renderText({ current_question()$Question })
      submit_disabled(FALSE)  # Дозволяємо натискання кнопки "Відправити відповідь"
      
      # Оновлення варіантів відповіді
      output$answer_choices <- renderUI({
        question <- current_question()
        # Формування списку варіантів відповіді з текстом
        choices <- setNames(
          c("A", "B", "C", "D"),
          c(paste("A:", question$A),
            paste("B:", question$B),
            paste("C:", question$C),
            paste("D:", question$D))
        )
        radioButtons("user_response", "Виберіть відповідь:", choices = choices)
      })
      
      output$feedback <- renderText("")  # Очищення тексту пояснення
    }
  })
  
}

# Запуск додатка Shiny
shinyApp(ui = ui, server = server)
