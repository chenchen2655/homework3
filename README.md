#include <iostream>
#include <string>
#include <vector>
#include <queue>
#include <map>
#include <algorithm>
using namespace std;

// 定义二叉树节点类BinNode
template <typename T>
class BinNode {
public:
    T data; // 数据域
    BinNode<T>* parent; // 父节点指针
    BinNode<T>* lc; // 左孩子指针
    BinNode<T>* rc; // 右孩子指针
    int height; // 高度

    // 构造函数
    BinNode(T d = T(), BinNode<T>* p = nullptr, BinNode<T>* l = nullptr, BinNode<T>* r = nullptr, int h = 0) :
        data(d), parent(p), lc(l), rc(r), height(h) {}

    // 判断是否有左孩子
    bool hasLChild() const {
        return lc != nullptr;
    }

    // 判断是否有右孩子
    bool hasRChild() const {
        return rc != nullptr;
    }

    // 判断是否为叶节点
    bool isLeaf() const {
        return !hasLChild() && !hasRChild();
    }

    // 返回节点的度（孩子数）
    int degree() const {
        return (hasLChild() ? 1 : 0) + (hasRChild() ? 1 : 0);
    }

    // 返回节点的兄弟节点（如果有）
    BinNode<T>* sibling() const {
        if (!parent) return nullptr; // 没有父节点，返回空指针
        return this == parent->lc ? parent->rc : parent->lc; // 返回父节点的另一个孩子
    }
};

// 定义二叉树类BinTree
template <typename T>
class BinTree {
protected:
    int _size; // 节点数
    BinNode<T>* _root; // 根节点指针

public:
    // 构造函数
    BinTree() : _size(0), _root(nullptr) {}

    // 析构函数
    ~BinTree() {
        clear(); // 清空树
    }

    // 返回树的规模（节点数）
    int size() const {
        return _size;
    }

    // 判断树是否为空
    bool empty() const {
        return !_root;
    }

    // 返回根节点指针
    BinNode<T>* root() const {
        return _root;
    }

    // 设置根节点指针
    void setRoot(BinNode<T>* r) {
        _root = r;
        if (_root) _root->parent = nullptr;
    }

    // 插入左孩子节点，返回新插入的节点指针
    BinNode<T>* insertAsLC(BinNode<T>* x, const T& e) {
        _size++; // 节点数增加
        x->lc = new BinNode<T>(e, x); // 创建新节点作为x的左孩子
        updateHeightAbove(x); // 更新x及其祖先的高度
        return x->lc; // 返回新插入的节点指针
    }

    // 插入右孩子节点，返回新插入的节点指针
    BinNode<T>* insertAsRC
        // 继续写代码
        (BinNode<T>* x, const T& e) {
        _size++; // 节点数增加
        x->rc = new BinNode<T>(e, x); // 创建新节点作为x的右孩子
        updateHeightAbove(x); // 更新x及其祖先的高度
        return x->rc; // 返回新插入的节点指针
    }

    // 删除以x为根的子树，返回被删除的节点数
    int remove(BinNode<T>* x) {
        if (!x) return 0; // 空树直接返回
        int n = 1 + remove(x->lc) + remove(x->rc); // 递归删除左右子树，并累计被删除的节点数
        delete x; // 删除当前节点
        _size -= n; // 更新树的规模
        return n; // 返回被删除的节点数
    }

    // 清空树，释放所有节点
    void clear() {
        if (_size > 0) {
            remove(_root); // 删除以根为根的子树
            _root = nullptr; // 根指针置空
        }
    }

    // 更新节点x及其祖先的高度
    void updateHeightAbove(BinNode<T>* x) {
        while (x) { // 从x向上，逐层更新高度
            updateHeight(x); // 更新当前节点的高度
            x = x->parent; // 移动到父节点
        }
    }

    // 更新节点x的高度，假设其孩子的高度已知
    void updateHeight(BinNode<T>* x) {
        int hl = x->hasLChild() ? x->lc->height : -1; // 左孩子的高度，如果没有则为-1
        int hr = x->hasRChild() ? x->rc->height : -1; // 右孩子的高度，如果没有则为-1
        x->height = 1 + max(hl, hr); // 当前节点的高度为孩子高度的最大值加一
    }

    // 前序遍历二叉树，对每个节点执行visit操作（函数指针或函数对象）
    template <typename VST>
    void travPre(VST& visit) {
        if (_root) travPre(_root, visit); // 从根节点开始遍历
    }

    // 中序遍历二叉树，对每个节点执行visit操作（函数指针或函数对象）
    template <typename VST>
    void travIn(VST& visit) {
        if (_root) travIn(_root, visit); // 从根节点开始遍历
    }

    // 后序遍历二叉树，对每个节点执行visit操作（函数指针或函数对象）
    template <typename VST>
    void travPost(VST& visit) {
        if (_root) travPost(_root, visit); // 从根节点开始遍历
    }

protected:
    // 前序遍历以x为根的子树，递归实现
    template <typename VST>
    static void travPre(BinNode<T>* x, VST& visit) {
        if (!x) return; // 空树直接返回
        visit(x->data); // 访问当前节点数据域
        travPre(x->lc, visit); // 遍历左子树
        travPre(x->rc, visit); // 遍历右子树
    }

    // 中序遍历以x为根的子树，递归实现
    template <typename VST>
    static void travIn(BinNode<T>* x, VST& visit) {
        if (!x) return; // 空树直接返回
        travIn(x->lc, visit); // 遍历左子树
        visit(x->data); //
        travIn(x->rc, visit); // 遍历右子树
    }

    // 后序遍历以x为根的子树，递归实现
    template <typename VST>
    static void travPost(BinNode<T>* x, VST& visit) {
        if (!x) return; // 空树直接返回
        travPost(x->lc, visit); // 遍历左子树
        travPost(x->rc, visit); // 遍历右子树
        visit(x->data); // 访问当前节点数据域
    }
};

// （2）基于BinTree构建HuffTree
// 定义Huffman树节点类HuffNode，继承自BinNode，增加权重和编码两个成员变量
class HuffNode : public BinNode<char> {
public:
    int weight; // 权重，即字符出现的频率
    string code; // 编码，即字符对应的二进制串

    // 构造函数
    HuffNode(char d = '\0', int w = 0, HuffNode* p = nullptr, HuffNode* l = nullptr, HuffNode* r = nullptr, int h = 0) :
        BinNode<char>(d, p, l, r, h), weight(w), code("") {}
};

// 定义Huffman树类HuffTree，继承自BinTree，增加构造函数和编码函数
class HuffTree : public BinTree<char> {
public:
    // 构造函数，根据字符及其频率数组构建Huffman树
    HuffTree(const vector<char>& chars, const vector<int>& freqs) {
        priority_queue<HuffNode*, vector<HuffNode*>, HuffNodeComparator> pq; // 定义一个优先队列，用于存放Huffman树节点，按照权重从小到大排序
        for (int i = 0; i < chars.size(); i++) { // 遍历字符数组和频率数组
            HuffNode* node = new HuffNode(chars[i], freqs[i]); // 创建一个新的Huffman树节点，存放字符和频率
            pq.push(node); // 将节点加入优先队列中
        }
        while (pq.size() > 1) { // 当优先队列中还有至少两个节点时，循环执行以下操作
            HuffNode* lc = pq.top(); pq.pop(); // 取出权重最小的节点作为左孩子
            HuffNode* rc = pq.top(); pq.pop(); // 取出权重次小的节点作为右孩子
            HuffNode* parent = new HuffNode('\0', lc->weight + rc->weight); // 创建一个新的父节点，其权重为左右孩子权重之和，字符为空
            parent->lc = lc; lc->parent = parent; // 将左孩子连接到父节点上
            parent->rc = rc; rc->parent = parent; // 将右孩子连接到父节点上
            pq.push(parent); // 将父节点加入优先队列中
        }
        _root = pq.top(); pq.pop(); // 最后优先队列中只剩一个节点，即为Huffman树的根节点
    }

    // 编码函数，根据Huffman树生成每个字符对应的二进制串，并存放在map中返回
    map<char, string> encode() {
        map<char, string> codes; // 定义一个map，用于存放字符和编码的映射关系
        encode(_root, codes); // 调用递归函数，从根节点开始编码，并将结果存入map中
        return codes; // 返回map
    }

protected:
    // 定义一个比较器类，用于比较两个Huffman树节点的权重大小
    class HuffNodeComparator {
    public:
        bool operator()(const HuffNode* a, const HuffNode* b) const {
            return a->weight > b->weight; // 如果a的权重大于b的权重，返回true，表示a的优先级低于b
        }
    };

    // 编码递归函数，根据Huffman树节点生成其对应的二进制串，并存放在map中
    static void encode(HuffNode* x, map<char, string>& codes) {
        if (!x) return; // 空节点直接返回
        if (x->isLeaf()) { // 如果是叶节点，说明是一个字符
            codes[x->data] = x->code; // 将字符和编码的映射关系存入map中
            return; // 返回
        }
        if (x->hasLChild()) { // 如果有左孩子
            x->lc->code = x->code + "0"; // 左孩子的编码为当前节点的编码加上0
            encode(x->lc, codes); // 递归编码左孩子
        }
        if (x->hasRChild()) { // 如果有右孩子
            x->rc->code = x->code + "1"; // 右孩子的编码为当前节点的编码加上1
            encode(x->rc, codes); // 递归编码右孩子
        }
    }
};

// （3）借助位图类Bitmap定义Huffman二进制编码串类型HuffCode
// 定义位图类Bitmap，用于存放和操作二进制位
class Bitmap {
private:
    char* M; // 位图所占的空间，每个字节存放8个二进制位
    int N; // 位图的容量，即最多能存放多少个二进制位

protected:
    // 初始化位图，将所有位都置为0
    void init(int n) {
        M = new char[N = (n + 7) / 8]; // 根据容量n计算所需的字节数，并分配空间
        memset(M, 0, N); // 将所有字节都置为0
    }

public:
    // 构造函数，根据指定的容量或者字符串初始化位图
    Bitmap(int n = 8) { init(n); } // 默认容量为8位
    Bitmap(char* s) { init(8 * strlen(s)); memcpy(M, s, N); } // 根据字符串初始化

    // 析构函数，释放空间
    ~Bitmap() { delete[] M; M = nullptr; }

    // 将位图中第k位（从0开始）置为1
    void set(int k) {
        expand(k); // 检查是否需要扩容
        M[k >> 3] |= (0x80 >> (k & 0x07)); // 找到第k位所在的字节，将相应的二进制位置为1
    }

    // 将位图中第k位（从0开始）清除为0
    void clear(int k) {
        expand(k); // 检查是否需要扩容
        M[k >> 3] &= ~(0x80 >> (k & 0x07)); // 找到第k位所在的字节，将相应的二进制位清除为0
    }

    // 判断位图中第k位（从0开始）是否为1，返回布尔值
    bool test(int k) {
        expand(k); // 检查是否需要扩容
        return M[k >> 3] & (0x80 >> (k & 0x07)); // 找到第k位所在的字节，判断相应的二进制位是否为1
    }

protected:
    // 检查是否需要扩容，如果第k位超出了当前容量，则进行扩容，并将多出来的位都置为0
    void expand(int k// 继续写代码
    ) {
        if (k < 8 * N) return; // 如果第k位没有超出当前容量，直接返回
        int oldN = N; // 记录旧的字节数
        char* oldM = M; // 记录旧的空间指针
        init(2 * k); // 重新初始化位图，容量扩大为原来的两倍
        memcpy(M, oldM, oldN); // 将旧的空间中的内容复制到新的空间中
        delete[] oldM; // 释放旧的空间
    }
};

// 定义Huffman二进制编码串类型HuffCode，继承自Bitmap，增加长度和输出操作符重载
class HuffCode : public Bitmap {
private:
    int _size; // 编码串的长度，即有效的二进制位数

public:
    // 构造函数，根据字符串或者长度初始化编码串
    HuffCode(char* s) : Bitmap(s), _size(8 * strlen(s)) {} // 根据字符串初始化
    HuffCode(int n = 8) : Bitmap(n), _size(0) {} // 根据长度初始化，默认为8位

    // 返回编码串的长度
    int size() const {
        return _size;
    }

    // 重载[]操作符，用于访问或修改编码串中的某一位
    bool operator[](int k) const {
        return test(k); // 调用Bitmap中的test函数，返回第k位的值
    }
    bool& operator[](int k) {
        _size = max(_size, k + 1); // 更新编码串的长度，取当前长度和k+1中的较大值
        return Bitmap::operator[](k); // 调用Bitmap中的[]操作符，返回第k位的引用
    }

    // 重载<<操作符，用于输出编码串到输出流中
    friend ostream& operator<<(ostream& os, const HuffCode& code) {
        for (int i = 0; i < code._size; i++) { // 遍历编码串中的每一位
            os << code[i]; // 输出该位的值，0或1
        }
        return os; // 返回输出流对象
    }
};

// （4）实现Huffman编码算法
// 定义一个函数，根据给定的文本内容，构建Huffman树，并返回字符和编码的映射关系
map<char, string> buildHuffmanTree(const string& text) {
    map<char, int> freqs; // 定义一个map，用于存放字符和频率的映射关系
    for (char c : text) { // 遍历文本中的每个字符
        freqs[c]++; // 将该字符对应的频率加一
    }
    vector<char> chars; // 定义一个向量，用于存放不同的字符
    vector<int> weights; // 定义一个向量，用于存放对应字符的频率（权重）
    for (auto& p : freqs) { // 遍历map中的每个键值对
        chars.push_back(p.first); // 将字符加入向量chars中
        weights.push_back(p.second); // 将频率加入向量weights中
    }
    HuffTree tree(chars, weights); // 根据字符和频率向量构建Huffman树
    return tree.encode(); // 返回Huffman树生成的编码映射关系
}

// 定义一个函数，根据给定的文本内容和编码映射关系，将文本转换为Huffman编码串，并返回
HuffCode encodeText(const string& text, const map<char, string>& codes) {
    HuffCode code(text.size() * 8); // 定义一个Huffman编码串，初始长度为文本长度乘以8（假设每个字符
    // 继续写代码
        int k = 0; // 定义一个变量，用于记录编码串中的当前位置
    for (char c : text) { // 遍历文本中的每个字符
        string s = codes.at(c); // 根据编码映射关系，找到该字符对应的二进制串
        for (char b : s) { // 遍历该二进制串中的每一位
            code[k++] = (b == '1'); // 将该位的值（0或1）存入编码串中的相应位置
        }
    }
    return code; // 返回编码串
}

// 定义一个函数，根据给定的Huffman编码串和编码映射关系，将编码串还原为文本内容，并返回
string decodeText(const HuffCode& code, const map<char, string>& codes) {
    string text; // 定义一个字符串，用于存放还原后的文本内容
    string s; // 定义一个字符串，用于存放当前匹配的二进制串
    for (int i = 0; i < code.size(); i++) { // 遍历编码串中的每一位
        s += (code[i] ? '1' : '0'); // 将该位的值（0或1）加入字符串s中
        for (auto& p : codes) { // 遍历编码映射关系中的每个键值对
            if (p.second == s) { // 如果找到了与s相同的二进制串
                text += p.first; // 将对应的字符加入文本内容中
                s.clear(); // 清空字符串s，准备下一次匹配
                break; // 跳出循环，继续遍历编码串
            }
        }
    }
    return text; // 返回文本内容
}

// 测试代码
int main() {
    string text = "Ihaveadream"; // 定义一个文本内容，为马丁路德金的演讲原文《Ihaveadream》
    cout << "Original text: " << text << endl; // 输出原始文本内容
    map<char, string> codes = buildHuffmanTree(text); // 构建Huffman树，并返回编码映射关系
    cout << "Huffman codes: " << endl; // 输出Huffman编码
    for (auto& p : codes) { // 遍历编码映射关系中的每个键值对
        cout << p.first << ": " << p.second << endl; // 输出字符和对应的二进制串
    }
    HuffCode code = encodeText(text, codes); // 将文本内容转换为Huffman编码串
    cout << "Encoded text: " << code << endl; // 输出编码后的文本内容（二进制串）
    string decoded = decodeText(code, codes); // 将Huffman编码串还原为文本内容
    cout << "Decoded text: " << decoded << endl; // 输出还原后的文本内容
    return 0;
}
