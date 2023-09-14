#include "AST.h"
#include "FindTarget.h"
#include "Selection.h"
#include "refactor/Tweak.h"
#include "support/Logger.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclBase.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/Expr.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Tooling/Core/Replacement.h"
#include <optional>

namespace clang {
namespace clangd {
namespace {

class RemoveUnusedVariable : public Tweak {
public:
  const char *id() const override;

  bool prepare(const Selection &Inputs) override;
  Expected<Effect> apply(const Selection &Inputs) override;
  std::string title() const override { return "Remove unused variable"; }
  llvm::StringLiteral kind() const override {
    return CodeAction::REFACTOR_KIND;
  }

private:
  const VarDecl *TargetDirective = nullptr;
};
REGISTER_TWEAK(RemoveUnusedVariable)

bool RemoveUnusedVariable::prepare(const Selection &Inputs) {
  // Find the variable under the cursor.
  auto *CA = Inputs.ASTSelection.commonAncestor();
  if (!CA) {
    return false;
  }
  TargetDirective = CA->ASTNode.get<VarDecl>();
  if (!TargetDirective) {
    return false;
  }
  if (!isa<Decl>(TargetDirective->getDeclContext())) {
    return false;
  }
  if (TargetDirective->isUsed()) {
    return false;
  }
  llvm::errs() << "This action is available\n";
  return true;
}

/// Produce edit removing 'using namespace xxx::yyy' and the trailing semicolon.
llvm::Expected<tooling::Replacement> removeVarDecl(ASTContext &Ctx,
                                                   const VarDecl *D) {
  auto &SM = Ctx.getSourceManager();
  std::optional<Token> NextTok =
      Lexer::findNextToken(D->getEndLoc(), SM, Ctx.getLangOpts());
  if (!NextTok || NextTok->isNot(tok::semi))
    return error("no semicolon after variable declaration");
  return tooling::Replacement(
      SM,
      CharSourceRange::getTokenRange(D->getBeginLoc(), NextTok->getLocation()),
      "", Ctx.getLangOpts());
}

Expected<Tweak::Effect> RemoveUnusedVariable::apply(const Selection &Inputs) {
  auto &Ctx = Inputs.AST->getASTContext();
  auto &SM = Ctx.getSourceManager();

  // Produce replacements to remove the unused variable decl directives.
  tooling::Replacements R;
  if (TargetDirective) {
    auto RemoveVar = removeVarDecl(Ctx, TargetDirective);
    if (!RemoveVar)
      return RemoveVar.takeError();
    if (auto Err = R.add(*RemoveVar))
      return std::move(Err);
  }
  return Effect::mainFileEdit(SM, std::move(R));
}

} // namespace
} // namespace clangd
} // namespace clang
