#include "mpi.h"
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <stack>
#include <omp.h>
#include "settings.cpp"

using namespace std;


string find_index(string qterm)
{
    int terminator = qterm.find('=');
    string result_index = qterm.substr(0,terminator);
    return result_index;
}

void output_result (double* result, short* qterm_flags, const string* qdet)
{
    ofstream out;
    out.open(OUTPUTH_PATH.c_str());
    string index;
    if (out.is_open())
    {
        for (int i=0;i<QTERM_COUNT;i++)
        {
            if (qterm_flags[i])
            {
                index = find_index(qdet[i]);
                out << index << " = " << result[i] << endl;
            }
        }
    }
    out.close();
}

void input_qdet(string* qdet, string path)
{
    ifstream fin;
    fin.open(path.c_str());
    string temp;
    int i =0;
    while (!fin.eof()) {
        getline(fin, temp);
        if (temp.empty())
        {
            break;
        }
        qdet[i] = temp;
        i++;
    }
}


void read_input_data(double** input, string path)
{
    ifstream fin;
    fin.open(path.c_str());
    string temp;
    int split_pos, i=0, j=0;
    while (!fin.eof()) {
        getline(fin, temp);
        if (temp.empty())
        {
            break;
        }
        temp.push_back(';');
        j=0;
        while (true) {
            split_pos = temp.find(";");
            if (split_pos == -1) {
                break;
            }
            input[i][j] = (std::atof(temp.substr(0, split_pos).c_str()));
            temp = temp.substr(split_pos + 1);
            j++;
        }
        i++;
    }
    fin.close();
}

double calculate_qterm(double** A, double** B, string qterm, bool qterm_type)   // 0 - log qterm, 1 - math qterm
{
    stack <char> oper_stack;
    stack <double> fo_stack;
    stack <double> so_stack;
    stack <int> order_stack;

    enum States
    {
        State_Start,
        State_Operand,
    };
    States state = State_Start;

    int qterm_size = qterm.size();
    char data_index = 'N';
    string cur_number = "";
    double result, temp_operand;
    int first_index, second_index, terminator;
    bool simple_argument = false;
    int current_operand;

    for (int i=0; i<qterm_size; i++)
    {
        /*
        if (!oper_stack.empty())
            cout << "STACK: " << oper_stack.top() << "  ";
        else
            cout << "STACK: " << "emp" << "  ";
        if (!fo_stack.empty())
            cout << fo_stack.top() << "  ";
        else
            cout << "emp" << "  ";
        if (!so_stack.empty())
            cout << so_stack.top() << "  ";
        else
            cout << "emp" << "  ";

        if (!order_stack.empty())
            cout << order_stack.top() << endl;
        else
            cout << "emp" << endl;
        */


        const char current = qterm[i];
        //cout << "STATE:" << state << " CHAR:" << qterm[i] << endl;
        switch (state)
        {
            case State_Start:
                if (current == 'A' || current == 'B' || isdigit(current))
                {
                    state = State_Operand;
                    simple_argument = true;
                    i -=1 ;
                }
                else if (current == 'o')
                {
                    /* Сделаем коды для двусоставных операций
                     1 - >=;
                     2 - <=;
                     3 - ==;
                     4 - !=;
                     */
                    if (qterm[i+6] == '=')
                    {
                        switch (qterm[i+5])
                        {
                            case '>':
                                oper_stack.push('1');
                                break;

                            case '<':
                                oper_stack.push('2');
                                break;

                            case '=':
                                oper_stack.push('3');
                                break;

                            case '!':
                                oper_stack.push('4');
                                break;
                        }
                    }
                    else if (qterm[i+6] == '"')
                    {
                        oper_stack.push(qterm[i+5]);
                    }

                    i += 7;

                }
                else if (current == 'f')
                {
                    state = State_Operand;
                    current_operand = 1;
                    i += 3;
                }
                else if (current == 's')
                {
                    state = State_Operand;
                    current_operand = 2;
                    i += 3;
                }

                else if (current == '}')
                {
                    //cout << "STACKS SIZE: " << fo_stack.size() << oper_stack.size() << so_stack.size() << " ORDER " << order_stack.size()<< endl;
                    //cout << "STACKS: " << fo_stack.top() << oper_stack.top() << so_stack.top() << " ORDER " << order_stack.top()<< endl;
                    switch (oper_stack.top())
                    {
                        case '*':
                            result = fo_stack.top() * so_stack.top();
                            break;

                        case '+':
                            result = fo_stack.top() + so_stack.top();
                            break;

                        case '-':
                            result = fo_stack.top() - so_stack.top();
                            break;

                        case '/':
                            result = fo_stack.top() / so_stack.top();
                            break;

                        case '&':
                            result = fo_stack.top() && so_stack.top();
                            break;

                        case '>':
                            result = fo_stack.top() > so_stack.top();
                            break;

                        case '<':
                            result = fo_stack.top() < so_stack.top();
                            break;

                        case '1':
                            result = fo_stack.top() >= so_stack.top();
                            break;

                        case '2':
                            result = fo_stack.top() <= so_stack.top();
                            break;

                        case '3':
                            result = fo_stack.top() == so_stack.top();
                            break;

                        case '4':
                            result = fo_stack.top() != so_stack.top();
                            break;

                    }

                    if (!qterm_type && oper_stack.top() == '&')    // Если вычисляем логическую часть qterm
                    {
                        if (!result)
                        {
                            return 0;
                        }

                    }
                    oper_stack.pop();
                    fo_stack.pop();
                    so_stack.pop();


                    if (!order_stack.empty())
                    {
                        if (order_stack.top() == 1)   // Если в стеке порядка лежит 0 (первый операнд)
                        {
                            fo_stack.push(result);
                        }
                        else
                        {
                            so_stack.push(result);
                        }
                        order_stack.pop();
                    }
                }
                break;


            case State_Operand:
                if (current == 'A' || current == 'B')
                {
                    data_index = current;
                    i++;
                }
                else if (current == '{')
                {
                    i++;
                    order_stack.push(current_operand);
                    state = State_Start;
                }
                else if (current == ')' || current == '}' || (current == ',' && data_index == 'N'))
                {
                    if (current == '}')
                        i--;
                    state = State_Start;
                    terminator = cur_number.find(',');
                    if (terminator == -1)
                    {
                        first_index = stoi(cur_number) - 1;
                        if (data_index == 'A')
                            temp_operand = A[0][first_index];
                        else if (data_index == 'B')
                            temp_operand = B[0][first_index];
                        else if (data_index == 'N')
                            temp_operand = stod(cur_number);
                    }
                    else
                    {
                        first_index = stoi(cur_number.substr(0, terminator)) - 1;
                        second_index = stoi(cur_number.substr(terminator + 1)) - 1;
                        if (data_index == 'A')
                            temp_operand = A[first_index][second_index];
                        else if (data_index == 'B')
                            temp_operand = B[first_index][second_index];
                        else if (data_index == 'N')
                            temp_operand = stod(cur_number);
                    }

                    if (simple_argument)
                    {
                        return (temp_operand);
                    }
                    else if (current_operand == 1)     // current operand = 1 (first operand)
                    {
                        fo_stack.push(temp_operand);
                    }
                    else  // current operand = 2                    (second operand)
                    {
                        so_stack.push(temp_operand);
                    }

                    cur_number.clear();               // 2,1
                    data_index = 'N';
                    temp_operand = 0;
                    break;
                }

                else if (isdigit(current) || current == ',' || current =='.')
                {
                    cur_number += current;
                }
                break;
         }
    }
    return result;
}



int main(int argc, char **argv)
{
    clock_t start_prog = clock();
    int i;
    string* qdet = new string[QTERM_COUNT];
    input_qdet(qdet, INPUT_PATH);

    double** A = new double* [A_ROW_NUMBER];
    for(i = 0; i < A_ROW_NUMBER; ++i)
    {
        A[i] = new double[A_COLUMN_NUMBER];
    }
    if (USING_A)
        read_input_data(A, A_PATH);

    double** B = new double* [B_ROW_NUMBER];
    for(i = 0; i < B_ROW_NUMBER; ++i)
    {
        B[i] = new double[B_COLUMN_NUMBER];
    }
    if (USING_B)
        read_input_data(B, B_PATH);

    //cout << qdet[0] << endl;
    //cout << A[0][0] << endl;
    //cout << B[0][0] << endl;

    clock_t end_input = clock();

    int part;
    const int all_threads = THREADS_COUNT * PROCESS_COUNT;
    if (all_threads == 1)
        part = QTERM_COUNT;
    else if (QTERM_COUNT % all_threads)
        part = QTERM_COUNT / all_threads + 1;
    else
        part = QTERM_COUNT / all_threads;


    double* final_result = new double[QTERM_COUNT];
    short* qterm_flags = new short[QTERM_COUNT];

    double* temp_result = new double[QTERM_COUNT];
    short* temp_qterm_flags = new short[QTERM_COUNT];

    for (i=0; i<QTERM_COUNT; i++)
    {
        final_result[i] = 0;
        qterm_flags[i] = 0;
        temp_result[i] = 0;
        temp_qterm_flags[i] = 0;
    }

    int rank, size;
    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);


    #pragma omp parallel num_threads(THREADS_COUNT) //private(terminator,math_qterm,a,i)
    {
        double qterm_result;
        int terminator1, terminator2; // 1 делитель - '=', 2 - ';'
        string log_qterm, math_qterm, result_index;
        int thread_number = rank * THREADS_COUNT + omp_get_thread_num();
        //cout << "THIS IS THREAT: " << omp_get_thread_num() << endl;
        //cout << "PART:" << part << endl;

        for (int j = part * thread_number; j < part * (thread_number + 1) && j < QTERM_COUNT; j++) {

            //cout << "I:  " << i << endl;

            if (qdet[j].empty()) {
                break;
            }
            terminator1 = qdet[j].find('=');
            terminator2 = qdet[j].find(';');
            result_index = qdet[j].substr(0, terminator1);
            log_qterm = qdet[j].substr(terminator1 + 1, terminator2 - terminator1 - 1);
            math_qterm = qdet[j].substr(terminator2 + 1);

            //cout << "Calculating log qterm: " << log_qterm << endl;
            if (log_qterm != " ") {
                qterm_result = calculate_qterm(A, B, log_qterm, 0);
            } else {
                qterm_result = 1;
            }
            //cout << "LOG QTERM RESULT: " << result << endl;
            if (!qterm_result) {
                //cout << "FIND FALSE LOG QTERM: " << log_qterm << endl;
                continue;
                temp_qterm_flags[j] = 0;
            }
            //cout << "Calculating math qterm: " << math_qterm << endl;
            temp_qterm_flags[j] = 1;
            temp_result[j] = calculate_qterm(A, B, math_qterm, 1);
            //cout << "Calc elem:  " << j << " RES: " << temp_result[j] << " FLAG: " << temp_qterm_flags[j] << endl;
        }
        #pragma omp barrier
    }



    MPI_Barrier(MPI_COMM_WORLD);
    MPI_Reduce(temp_result, final_result, QTERM_COUNT, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);
    MPI_Reduce(temp_qterm_flags, qterm_flags, QTERM_COUNT, MPI_SHORT, MPI_SUM, 0, MPI_COMM_WORLD);
    MPI_Barrier(MPI_COMM_WORLD);
    MPI_Finalize();

    clock_t end_math = clock();

    output_result(final_result, qterm_flags, qdet);
    clock_t end_prog = clock();

    double reading_time = (double)(end_input - start_prog) / CLOCKS_PER_SEC;
    double math_time = (double)(end_math - end_input) / CLOCKS_PER_SEC;
    double output_time = (double)(end_prog - end_math) / CLOCKS_PER_SEC;

    ofstream log;
    log.open(LOG_PATH.c_str());
    if (log.is_open())
    {
        log << "READING TIME " << reading_time << endl;
        log << "CALCULATING TIME " << math_time << endl;
        log << "OUTPUT TIME " << output_time << endl;
        log << "EXEXUTION TIME " << reading_time + math_time + output_time << endl;
    }
    log.close();

    delete[] qdet;
    for (i=0;i<A_COLUMN_NUMBER;i++)
        delete[] A[i];
    delete[] A;

    for (i=0;i<B_COLUMN_NUMBER;i++)
        delete[] B[i];
    delete[] B;

    delete[] qterm_flags;
    delete[] final_result;
    delete[] temp_qterm_flags;
    delete[] temp_result;

    return 0;
}
