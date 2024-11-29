using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.IO;

namespace VchMathSh
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }

        double searchP(double[,] array, int size)  //Коэффициенты Р
        {
            double tempP = 0;
            for (int index1 = 0; index1 < size; index1++)
            {
                for (int index2 = 0; index2 < size; index2++)
                {
                    if (index1 == index2)
                        tempP += array[index1,index2];
                }
            }
            return tempP;
        }

        double[,] mult(double[,] mass, double[,] mass2, int size) //Умножение матриц
        {
            double[,] mass3 = new double [size,size];
            for (int i = 0; i < size; i++)
            {
                for (int j = 0; j < size; j++)
                {
                    mass3[i,j] = 0;
                    for (int k = 0; k < size; k++)
                    {
                        mass3[i,j] += mass[i,k] * mass2[k,j];
                    }
                }
            }
            return mass3;
        }


        double[,] subs(double[,] array1, double[,] array2, int size, double[,] arrayRez) //Разность матриц
        {
            for (int index1 = 0; index1 < size; index1++)
            {
                for (int index2 = 0; index2 < size; index2++)
                {
                    {
                        arrayRez[index1,index2] = array1[index1,index2] - array2[index1,index2];
                    }
                }
            }
            return arrayRez;
        }

        double[,] multNum(double[,] arr, double P, int size) // Матрица PE (Унможение матрицы на число)
        {
            for (int index1 = 0; index1 < size; index1++)  
            {
                for (int index2 = 0; index2 < size; index2++)
                {
                    {
                        arr[index1,index2] = P * arr[index1,index2];
                    }
                }
            }

            return arr;
        }

        double[] lambda(int size, double[,] a, double[] p) //Нахождение собственных значений до функции (лямбда, ньютон, фун, матрХкоеф)
        {
            double tmp1 = a[0, 0];
            double tmp2 = a[0, 0];
            for (int index11 = 0; index11 < size; ++index11)
            {
                double num = 0.0;
                for (int index2 = 0; index2 < size; ++index2)
                {
                    if (index11 != index2)
                        num += Math.Abs(a[index11, index2]);
                }
                if (a[index11, index11] - num < tmp2)
                    tmp2 = a[index11, index11] - num;
                if (a[index11, index11] + num > tmp1)
                    tmp1 = a[index11, index11] + num;
            }
            for (int index = 1; index < p.Length; ++index)
                p[index] = p[index] * -1.0;

            double tmpnum = 10.0;
            int index1 = 0;
            double[] arrNum = new double[size];
            for (int index2 = 0; (double)index2 < tmpnum; ++index2)
            {
                double x0_1 = tmp2 + (tmp1 - tmp2) * ((double)index2 / tmpnum);
                double x0_2 = tmp2 + (tmp1 - tmp2) * ((double)(index2 + 1) / tmpnum);
                if (fun(p, x0_1) * fun(p, x0_2) <= 0.0)
                {
                    double x1 = (x0_1 + x0_2) / 2.0;
                    arrNum[index1] = newton(p, x1, 0.000001);
                    ++index1;
                }
            }
            return ((IEnumerable<double>)arrNum).OrderBy<double, double>((Func<double, double>)(x => x)).ToArray<double>();
        }


        double fun(double[] koef, double x0)
        {
            double x = x0;
            double num = 0.0;
            double[] arrNum = new double[koef.Length];
            double length = (double)arrNum.Length;
            for (int index = 0; index < arrNum.Length; ++index)
            {
                --length;
                arrNum[index] = length;
            }
            for (int index = 0; index < koef.Length; ++index)
                num += koef[index] * Math.Pow(x, arrNum[index]);
            return num;
        }

        double newton(double[] x, double x1, double eps)
        {
            int num = 0;
            for (; Math.Abs(fun(x, x1)) > eps; x1 -= fun(x, x1) / fun(matrXkoef(x), x1))
            {
                ++num;
                if (num < 10000)
                {
                    if (Math.Abs(fun(matrXkoef(x), x1)) < eps)
                    {
                        return 101.0;
                    }
                }
                else
                    break;
            }
            return x1;
        }

        double[] matrXkoef(double[] size)
        {
            double[] arrNum = new double[size.Length];
            double length = (double)arrNum.Length;
            for (int index = 0; index < arrNum.Length; ++index)
            {
                --length;
                arrNum[index] = length;
            }
            double[] source = new double[size.Length];
            for (int index = 0; index < arrNum.Length; ++index)
                source[index] = size[index] * arrNum[index];
            return ((IEnumerable<double>)source).Where<double>((Func<double, bool>)(x => x != 0.0)).ToArray<double>();
        }

        void iteration(double[,] arrayA, int num, int size, double p, double[,] arrayB) //Вывод расчетов в текстбокс
        {
            if (num == 1)
                textBox2.Text += "Итерация №" + Convert.ToString(num) + ":" + Environment.NewLine;
            else
                textBox2.Text += Environment.NewLine + Environment.NewLine + "Итерация №" + System.Convert.ToString(num) + ":" + Environment.NewLine;
            textBox2.Text += Environment.NewLine + "Матрица А [" + System.Convert.ToString(num) + "] :";

            for (int index1 = 0; index1 < size; index1++)
            {
                textBox2.Text += Environment.NewLine;
                for (int index2 = 0; index2 < size; index2++)
                {
                    {
                        textBox2.Text += System.Convert.ToString(Math.Round(arrayA[index1,index2], 3)) + "  ";

                    }
                }
            }

            textBox2.Text += Environment.NewLine + Environment.NewLine + "Число P = " + System.Convert.ToString(p) + Environment.NewLine;

            textBox2.Text += Environment.NewLine + "Матрица B [" + System.Convert.ToString(num) + "] :";

            for (int index1 = 0; index1 < size; index1++)
            {
                textBox2.Text += Environment.NewLine;
                for (int index2 = 0; index2 < size; index2++)
                {
                    {
                        textBox2.Text += System.Convert.ToString(Math.Round(arrayB[index1,index2], 3)) + "  ";

                    }
                }
            }
        }

        void vectors(double lmbd, int index, int x, int size, double[,] arrBi)  //Поиск векторов
        {
            double[] arrY1 = new double[size];
            double[] arrY2 = new double[size];
            double[] arrX = new double[size];
            double[] arrB = new double[size];
            double[] arrY0 = new double[size];
            double sumkv=0;
            textBox2.Text += Environment.NewLine + Environment.NewLine;
            arrY0[0] = 1;

            for (int index2=1; index2<size; index2++)
            {
                textBox2.Text += " Y" + index2 + ": " + Environment.NewLine;
                for (int i = 0; i < size; i++)
                {
                    arrB[i] = arrBi[i, index2];
                }
                arrY1 = add(multNumEd(arrY0, lmbd, size), arrB, size);
                for (int i = 0; i < size; i++)
                {
                    textBox2.Text += Math.Round(arrY1[i], x) + " ";
                    arrY0 = arrY1;
                    if (index2 == size - 1)
                        sumkv += arrY1[i] * arrY1[i];

                }
            }
            textBox2.Text += Environment.NewLine + Environment.NewLine + "Нормируем вектор на его длину:" + Environment.NewLine;
            for (int i = 0; i < size; i++)
            {
                textBox2.Text += Math.Round((arrY1[i] / Math.Sqrt(sumkv)), x) + " ";
                dataGridView2.Rows[i].Cells[index].Value = Math.Round((arrY1[i] / Math.Sqrt(sumkv)), x);
            }

        }

        double[] multNumEd(double[] arr, double P, int size) //Умножение одномерной матрицы на число
        {
            for (int index1 = 0; index1 < size; index1++)
            {
                        arr[index1] = P * arr[index1];
            }

            return arr;
        }

        double[] add(double[] array1, double[] array2, int size) //Сумма матриц
        {
            double[] arrayRez = new double[size];
            for (int index1 = 0; index1 < size; index1++)
            {
                        arrayRez[index1] = array1[index1]+ array2[index1];
            }
            return arrayRez;
        }

        private void textBox2_TextChanged(object sender, EventArgs e)
        {
            textBox2.SelectionStart = textBox2.Text.Length;
            textBox2.ScrollToCaret();
            textBox2.Refresh();
        }

        private void numericUpDown1_ValueChanged(object sender, EventArgs e)
        {
            dataGridView1.ColumnCount = Convert.ToInt32(numericUpDown1.Value);
            dataGridView1.RowCount = Convert.ToInt32(numericUpDown1.Value);
            for (int i = 0; i < Convert.ToInt32(numericUpDown1.Value); i++)
            {
                for (int j = 0; j < Convert.ToInt32(numericUpDown1.Value); j++)
                {
                    if (i < j)
                    {
                        dataGridView1.Rows[i].Cells[j].ReadOnly = true;
                        dataGridView1.Rows[i].Cells[j].Style.BackColor = Color.LightGray;
                    }
                    if (i==j)
                        dataGridView1.Rows[i].Cells[j].Style.BackColor = Color.LightBlue;
                }
            }

            dataGridView2.ColumnCount = Convert.ToInt32(numericUpDown1.Value);
            dataGridView2.RowCount = Convert.ToInt32(numericUpDown1.Value);
            if (numericUpDown1.Value != 0)
            {
                for (int index = 1; index <= numericUpDown1.Value; index++)
                {
                    dataGridView2.Columns[index - 1].HeaderCell.Value = "X" + index;
                }
            }
        }

        private void загрузитьToolStripMenuItem_Click(object sender, EventArgs e)
        {
            Stream str = null;
            if (openFileDialog1.ShowDialog() == DialogResult.OK)
            {
                if ((str = openFileDialog1.OpenFile())!= null)
                {
                    StreamReader read = new StreamReader(str);
                    string[] strg;
                    int index = 0;
                    try
                    { 
                        string[] strg1 = read.ReadToEnd().Split('\n');
                        numericUpDown1.Value = Convert.ToInt32(strg1[0]);
                        index = strg1.Count();
                        dataGridView1.RowCount = index-1;
                        for (int i=1; i < index; i++)
                        {
                            strg = strg1[i].Split(' ');
                            for (int j = 0; j < dataGridView1.ColumnCount; j++)
                            {
                                try
                                {
                                    dataGridView1.Rows[i-1].Cells[j].Value = strg[j];
                                }
                                catch
                                {

                                }
                            }
                        }
                    }
                    catch (Exception ex)
                    {
                        MessageBox.Show(ex.Message);
                    }
                    finally
                    {
                        read.Close();
                    }
                }
            }

        }

        private void button1_Click(object sender, EventArgs e)
        {
            dataGridView1.Rows.Clear();
            dataGridView2.Rows.Clear();
            numericUpDown1.Value = 1;
            textBox1.Text = "";
            textBox2.Text = "";
            numericUpDown2.Value = 1;
        }

        private void button2_Click(object sender, EventArgs e)
        {

            try
            {
                int x = Convert.ToInt32(numericUpDown2.Value);
                int size = Convert.ToInt32(numericUpDown1.Value);
                double[,] arrA = new double[size, size];
                double[,] arrA1 = new double[size, size];
                double[,] arrB = new double[size, size];
                double[] arrP = new double[size + 1];
                double[] arrEig = new double[size];
                double[,] arrE = new double[size, size];
                double[,] arrPE = new double[size, size];
                double[,] arrBi = new double[size, size];
                double tempP;
                double[] arrY = new double[size];
                arrP[0] = 1;
                arrY[0] = 1;


                for (int index = 0; index < size; index++)
                {
                    arrBi[index, 1] = arrY[index];
                }

                for (int i = 0; i < size; i++) //Заполнение матрицы А и А1
                {
                    for (int j = 0; j < size; j++)
                    {
                        arrA[i, j] = Convert.ToDouble(dataGridView1.Rows[i].Cells[j].Value);
                    }
                }

                arrA1 = arrA;

                textBox2.Text = ""; //Очистка поля результатов
                textBox1.Text = ""; //Очистка поля собственных значений

                for (int i = 1; i < size + 1; i++) //Нахождение коэффицентов Р
                {

                    for (int y = 0; y < size; y++)
                    {
                        for (int j = 0; j < size; j++)
                        {
                            if (y == j)
                                arrE[y, j] = 1;
                            else arrE[y, j] = 0;
                        }
                    }
                    tempP = Math.Round(searchP(arrA1, size) / i, x);
                    arrPE = multNum(arrE, tempP, size);
                    arrB = subs(arrA1, arrPE, size, arrB);

                    if (i < size)
                    {
                        for (int index = 0; index < size; index++)
                        {
                            arrBi[index, i] = arrB[index, 0];
                        }
                    }
                    arrP[i] = tempP;

                    iteration(arrA1, i, size, tempP, arrB);

                    arrA1 = mult(arrA, arrB, size);

                }

                arrEig = lambda(size, arrA, arrP);

                textBox2.Text += Environment.NewLine + Environment.NewLine + "Корни характеристического уравнения:" + Environment.NewLine;

                for (int i = 0; i < size; i++) //Вывод собственных значений
                {
                    textBox2.Text += Math.Round(arrEig[i], x) + " ";
                    textBox1.Text += "L" + (i + 1) + " = " + Math.Round(arrEig[i], x) + Environment.NewLine;

                }


                textBox2.Text += Environment.NewLine + Environment.NewLine + "Вектор Y0: " + Environment.NewLine;

                for (int i = 0; i < size; i++) //Вывод вектора У0
                {
                    textBox2.Text += arrY[i] + " ";

                }

                textBox2.Text += Environment.NewLine;

                textBox2.Text += Environment.NewLine + "Матрица YB:";

                for (int index1 = 0; index1 < size; index1++)
                {
                    textBox2.Text += Environment.NewLine;
                    for (int index2 = 0; index2 < size; index2++)
                    {
                        {
                            textBox2.Text += System.Convert.ToString(Math.Round(arrBi[index1, index2], 3)) + "  ";

                        }
                    }
                }

                for (int index = 0; index < size; index++)
                {
                    vectors(arrEig[index], index, x, size, arrBi);
                }
            }
            catch (Exception ex)
            {
                MessageBox.Show("Проверьте введенную матрицу");
            }
        }

        private void пример1ToolStripMenuItem_Click(object sender, EventArgs e)
        {
            numericUpDown1.Value = 4;
            numericUpDown2.Value = 4;

            dataGridView1.Rows[0].Cells[0].Value = 2.2;
            dataGridView1.Rows[0].Cells[1].Value = 1;
            dataGridView1.Rows[0].Cells[2].Value = 0.5;
            dataGridView1.Rows[0].Cells[3].Value = 2;

            dataGridView1.Rows[1].Cells[0].Value = 1;
            dataGridView1.Rows[1].Cells[1].Value = 1.3;
            dataGridView1.Rows[1].Cells[2].Value = 2;
            dataGridView1.Rows[1].Cells[3].Value = 1;

            dataGridView1.Rows[2].Cells[0].Value = 0.5;
            dataGridView1.Rows[2].Cells[1].Value = 2;
            dataGridView1.Rows[2].Cells[2].Value = 0.5;
            dataGridView1.Rows[2].Cells[3].Value = 1.6;

            dataGridView1.Rows[3].Cells[0].Value = 2;
            dataGridView1.Rows[3].Cells[1].Value = 1;
            dataGridView1.Rows[3].Cells[2].Value = 1.6;
            dataGridView1.Rows[3].Cells[3].Value = 2;
        }

        private void пример2ToolStripMenuItem_Click(object sender, EventArgs e)
        {
            numericUpDown1.Value = 3;
            numericUpDown2.Value = 4;

            dataGridView1.Rows[0].Cells[0].Value = 2.6;
            dataGridView1.Rows[0].Cells[1].Value = 1.2;
            dataGridView1.Rows[0].Cells[2].Value = -0.1;

            dataGridView1.Rows[1].Cells[0].Value = 1.2;
            dataGridView1.Rows[1].Cells[1].Value = 2.1;
            dataGridView1.Rows[1].Cells[2].Value = 1.6;

            dataGridView1.Rows[2].Cells[0].Value = -0.1;
            dataGridView1.Rows[2].Cells[1].Value = 1.6;
            dataGridView1.Rows[2].Cells[2].Value = 0.8;
        }

        private void пример3ToolStripMenuItem_Click(object sender, EventArgs e)
        {
            numericUpDown1.Value = 2;
            numericUpDown2.Value = 3;
            dataGridView1.Rows[0].Cells[0].Value = 10;
            dataGridView1.Rows[0].Cells[1].Value = -25;

            dataGridView1.Rows[1].Cells[0].Value = -25;
            dataGridView1.Rows[1].Cells[1].Value = 10;
        }

        private void сохранитьToolStripMenuItem1_Click(object sender, EventArgs e)
        {
            if (saveFileDialog1.ShowDialog()== DialogResult.OK)
            {
                Name = saveFileDialog1.FileName + ".txt";
                File.WriteAllText(Name, textBox2.Text);
            }
        }

        private void сохранитьToolStripMenuItem_Click(object sender, EventArgs e)
        {
            Stream stream;
            if (saveFileDialog2.ShowDialog() == DialogResult.OK)
            {
                if ((stream = saveFileDialog2.OpenFile()) != null)
                {
                    StreamWriter writer = new StreamWriter(stream);
                    try
                    {
                        writer.WriteLine(numericUpDown1.Value);
                        for(int i = 0; i < dataGridView1.RowCount; i++)
                        {
                            for (int j = 0; j < dataGridView1.ColumnCount; j++)
                            {
                                writer.Write((dataGridView1.Rows[i].Cells[j].Value)+" ");
                            }
                            if(i!= dataGridView1.RowCount-1)
                            writer.WriteLine();
                        }
                    }
                    catch (Exception ex)
                    {
                        MessageBox.Show(ex.Message);
                    }
                    finally
                    {
                        writer.Close();
                    }
                    stream.Close();
                }
            }
        }


        private void dataGridView1_CellEndEdit(object sender, DataGridViewCellEventArgs e)
        {
            for (int i = 0; i < Convert.ToInt32(numericUpDown1.Value); i++)
            {
                for (int j = 0; j < Convert.ToInt32(numericUpDown1.Value); j++)
                {
                    if (i < j)
                        dataGridView1.Rows[i].Cells[j].Value = dataGridView1.Rows[j].Cells[i].Value;
                }
            }
        }

        private void openFileDialog1_FileOk(object sender, CancelEventArgs e)
        {

        }
    }
}
