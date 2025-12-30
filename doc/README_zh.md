📕 **中文** | 📘 [English](../README.md)

# SEC：土壤模型  
*基于 Fortran 的土壤碳过程模型*

---

## 一、项目概述

**SEC模型** 是一个基于 **Fortran** 实现的过程模型，用于模拟土壤—生态系统碳循环过程及其关键调控机制。

模型采用 **模块化结构设计**，将核心过程计算、输入输出接口与模型驱动程序相互分离，适用于 **高性能计算（HPC）环境** 下的大规模数值模拟与情景分析。

---

## 二、代码结构

SEC/
├── src/                  # 核心 Fortran 源代码
│   ├── main.f90
│   ├── mesc_cost.f90
│   ├── mesc_function.f90
│   ├── mesc_input.f90
│   ├── mesc_interface.f90
│   ├── mesc_model.f90
│   └── mesc_variable.f90
│
├── cmake/                # CMake 辅助模块
│   └── FindNetCDFFortran.cmake
│
├── test/                 # 测试与示例运行目录
│
├── CMakeLists.txt        # 构建配置
├── build.sh              # 一键构建脚本
├── README.md
└── LICENSE

---

## 三、运行环境与依赖

SEC 模型在以下软件环境中开发与测试：

- **Fortran 编译器**：NVHPC（`nvfortran`）版本 **25.11**
- **netCDF-Fortran**：版本 **4.6.1**
- **netCDF-C**：版本 **4.9.2**

在 HPC 集群环境中，相关依赖通常通过 module 系统加载，例如：

```bash
module load nvhpc
module load netcdf_hpc
```

---

## 四、编译方法

### 推荐方式：一键构建
```bash
chmod +x build.sh
./build.sh
```
该脚本将自动完成环境加载、模型配置与编译，并将生成的可执行文件复制至 **SEC/test/** 目录。

### 手动构建
```bash
module load nvhpc 
module load netcdf_hpc
mkdir build
cd build
cmake .. -DCMAKE_Fortran_COMPILER=nvfortran
cmake --build . -j
```

---

## 五、运行说明

模型需要输入文件（如参数文件或 netCDF 数据），建议将其统一放置于 test/ 目录中，并在该目录下运行模型。
```bash
cd SEC/test
./main
```
