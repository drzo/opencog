#pragma once
#include <string>
#include <vector>
#include <memory>
#include <unordered_map>
#include <functional>

// Tensor shape: e.g., {batch, latent_dim, time_steps}
using TensorShape = std::vector<size_t>;

class CognitiveSchema {
public:
    using Ptr = std::shared_ptr<CognitiveSchema>;
    using Properties = std::unordered_map<std::string, double>;

    CognitiveSchema(std::string id, std::string label, TensorShape shape, Properties props = {})
        : id_(std::move(id)), label_(std::move(label)), tensor_shape_(std::move(shape)), properties_(std::move(props)) {}

    void add_child(Ptr child) { children_.push_back(child); }
    const std::vector<Ptr>& children() const { return children_; }

    // Virtual: specialize for flow, insight, etc.
    virtual void activate(const std::vector<double>& input);

    // Attention scoring based on properties and context
    virtual double score(const Properties& context) const;

    // Expose shape for ggml
    const TensorShape& tensor_shape() const { return tensor_shape_; }
    const std::string& id() const { return id_; }
    const std::string& label() const { return label_; }
    const Properties& properties() const { return properties_; }

protected:
    std::string id_, label_;
    TensorShape tensor_shape_;
    Properties properties_;
    std::vector<Ptr> children_;
};