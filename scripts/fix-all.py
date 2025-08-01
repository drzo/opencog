#!/usr/bin/env python3
"""
OpenCog Master Fixing Tool
==========================

This is the main entry point for fixing various issues in the OpenCog project.
It orchestrates all the specialized fixing tools to provide a comprehensive
solution for:

- Test failures (C++, Python, Scheme)
- Build system issues (CMake)
- Runtime errors
- Configuration problems
- Dependency issues

The tool can run in automatic mode or interactive mode, and provides detailed
reports of all fixes applied.
"""

import os
import sys
import subprocess
import argparse
import json
import logging
from pathlib import Path
from typing import List, Dict, Tuple, Optional
from datetime import datetime
import concurrent.futures
from dataclasses import dataclass
from enum import Enum


class FixerType(Enum):
    """Types of fixers available"""
    TEST_FIXER = "test"
    CMAKE_FIXER = "cmake"
    SCHEME_FIXER = "scheme"
    ALL = "all"


@dataclass
class FixerResult:
    """Result from running a fixer"""
    fixer_type: FixerType
    success: bool
    issues_found: int
    issues_fixed: int
    error_message: Optional[str] = None
    log_file: Optional[Path] = None


class FixerOrchestrator:
    """Orchestrates all fixing tools"""
    
    def __init__(self, project_root: Path):
        self.project_root = project_root
        self.scripts_dir = project_root / "scripts"
        self.logs_dir = project_root / "fix-logs"
        self.logs_dir.mkdir(exist_ok=True)
        
        # Setup logging
        self._setup_logging()
        
        # Define available fixers
        self.fixers = {
            FixerType.TEST_FIXER: {
                "script": "fix-tests.py",
                "name": "Test Fixer",
                "description": "Fixes test failures in C++, Python, and Guile tests"
            },
            FixerType.CMAKE_FIXER: {
                "script": "fix-cmake.py",
                "name": "CMake Fixer",
                "description": "Fixes CMake build system issues"
            },
            FixerType.SCHEME_FIXER: {
                "script": "fix-scheme-tests.scm",
                "name": "Scheme Test Fixer",
                "description": "Fixes Scheme/Guile specific test issues"
            }
        }
    
    def _setup_logging(self):
        """Setup logging configuration"""
        log_file = self.logs_dir / f"fix-all-{datetime.now().strftime('%Y%m%d-%H%M%S')}.log"
        
        logging.basicConfig(
            level=logging.INFO,
            format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
            handlers=[
                logging.FileHandler(log_file),
                logging.StreamHandler(sys.stdout)
            ]
        )
        
        self.logger = logging.getLogger(self.__class__.__name__)
        self.logger.info(f"Starting OpenCog Master Fixing Tool")
        self.logger.info(f"Project root: {self.project_root}")
        self.logger.info(f"Log file: {log_file}")
    
    def run_fixer(self, fixer_type: FixerType, dry_run: bool = False, 
                  interactive: bool = False) -> FixerResult:
        """Run a specific fixer"""
        if fixer_type not in self.fixers:
            return FixerResult(
                fixer_type=fixer_type,
                success=False,
                issues_found=0,
                issues_fixed=0,
                error_message=f"Unknown fixer type: {fixer_type}"
            )
        
        fixer_info = self.fixers[fixer_type]
        script_path = self.scripts_dir / fixer_info["script"]
        
        if not script_path.exists():
            return FixerResult(
                fixer_type=fixer_type,
                success=False,
                issues_found=0,
                issues_fixed=0,
                error_message=f"Fixer script not found: {script_path}"
            )
        
        self.logger.info(f"Running {fixer_info['name']}...")
        
        # Build command
        cmd = []
        if script_path.suffix == ".py":
            cmd = [sys.executable, str(script_path)]
        elif script_path.suffix == ".scm":
            cmd = ["guile", str(script_path)]
        else:
            cmd = [str(script_path)]
        
        # Add arguments
        if dry_run:
            cmd.append("--dry-run")
        if interactive:
            cmd.append("--interactive")
        
        # Add project root
        cmd.extend(["--project-root", str(self.project_root)])
        
        # Create log file for this fixer
        log_file = self.logs_dir / f"{fixer_type.value}-{datetime.now().strftime('%Y%m%d-%H%M%S')}.log"
        
        try:
            # Run the fixer
            with open(log_file, 'w') as log:
                result = subprocess.run(
                    cmd,
                    cwd=self.project_root,
                    capture_output=True,
                    text=True,
                    timeout=3600  # 1 hour timeout
                )
                
                # Write output to log
                log.write(f"Command: {' '.join(cmd)}\n")
                log.write(f"Exit code: {result.returncode}\n")
                log.write(f"\n--- STDOUT ---\n{result.stdout}\n")
                log.write(f"\n--- STDERR ---\n{result.stderr}\n")
            
            # Parse results
            issues_found, issues_fixed = self._parse_fixer_output(result.stdout)
            
            return FixerResult(
                fixer_type=fixer_type,
                success=result.returncode == 0,
                issues_found=issues_found,
                issues_fixed=issues_fixed,
                error_message=result.stderr if result.returncode != 0 else None,
                log_file=log_file
            )
            
        except subprocess.TimeoutExpired:
            return FixerResult(
                fixer_type=fixer_type,
                success=False,
                issues_found=0,
                issues_fixed=0,
                error_message="Fixer timed out after 1 hour",
                log_file=log_file
            )
        except Exception as e:
            return FixerResult(
                fixer_type=fixer_type,
                success=False,
                issues_found=0,
                issues_fixed=0,
                error_message=str(e),
                log_file=log_file
            )
    
    def _parse_fixer_output(self, output: str) -> Tuple[int, int]:
        """Parse fixer output to extract statistics"""
        issues_found = 0
        issues_fixed = 0
        
        # Look for common patterns in output
        import re
        
        # Pattern for "Found X issues"
        found_match = re.search(r'(?:Found|Detected)\s+(\d+)\s+(?:issues?|failures?)', output, re.IGNORECASE)
        if found_match:
            issues_found = int(found_match.group(1))
        
        # Pattern for "Fixed X issues"
        fixed_match = re.search(r'(?:Fixed|Resolved)\s+(\d+)\s+(?:issues?|failures?)', output, re.IGNORECASE)
        if fixed_match:
            issues_fixed = int(fixed_match.group(1))
        
        return issues_found, issues_fixed
    
    def run_all_fixers(self, dry_run: bool = False, interactive: bool = False,
                       parallel: bool = True) -> Dict[FixerType, FixerResult]:
        """Run all fixers"""
        results = {}
        
        if parallel and not interactive:
            # Run fixers in parallel (not possible in interactive mode)
            with concurrent.futures.ThreadPoolExecutor(max_workers=3) as executor:
                future_to_fixer = {
                    executor.submit(self.run_fixer, fixer_type, dry_run, False): fixer_type
                    for fixer_type in self.fixers.keys()
                }
                
                for future in concurrent.futures.as_completed(future_to_fixer):
                    fixer_type = future_to_fixer[future]
                    try:
                        result = future.result()
                        results[fixer_type] = result
                    except Exception as e:
                        results[fixer_type] = FixerResult(
                            fixer_type=fixer_type,
                            success=False,
                            issues_found=0,
                            issues_fixed=0,
                            error_message=str(e)
                        )
        else:
            # Run fixers sequentially
            for fixer_type in self.fixers.keys():
                results[fixer_type] = self.run_fixer(fixer_type, dry_run, interactive)
        
        return results
    
    def generate_summary_report(self, results: Dict[FixerType, FixerResult]):
        """Generate a summary report of all fixes"""
        report_file = self.project_root / "fix-summary-report.md"
        
        with open(report_file, 'w') as f:
            f.write("# OpenCog Fix Summary Report\n\n")
            f.write(f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n\n")
            
            # Overall statistics
            total_found = sum(r.issues_found for r in results.values())
            total_fixed = sum(r.issues_fixed for r in results.values())
            successful_fixers = sum(1 for r in results.values() if r.success)
            
            f.write("## Overall Summary\n\n")
            f.write(f"- Total issues found: **{total_found}**\n")
            f.write(f"- Total issues fixed: **{total_fixed}**\n")
            f.write(f"- Success rate: **{total_fixed/total_found*100:.1f}%**\n")
            f.write(f"- Fixers run: **{len(results)}**\n")
            f.write(f"- Successful fixers: **{successful_fixers}**\n\n")
            
            # Individual fixer results
            f.write("## Fixer Results\n\n")
            
            for fixer_type, result in results.items():
                fixer_info = self.fixers.get(fixer_type, {})
                f.write(f"### {fixer_info.get('name', fixer_type.value)}\n\n")
                f.write(f"- **Status**: {'✓ Success' if result.success else '✗ Failed'}\n")
                f.write(f"- **Issues Found**: {result.issues_found}\n")
                f.write(f"- **Issues Fixed**: {result.issues_fixed}\n")
                
                if result.error_message:
                    f.write(f"- **Error**: {result.error_message}\n")
                
                if result.log_file:
                    f.write(f"- **Log File**: {result.log_file.relative_to(self.project_root)}\n")
                
                f.write("\n")
            
            # Recommendations
            f.write("## Recommendations\n\n")
            
            if total_fixed < total_found:
                f.write("### Unresolved Issues\n\n")
                f.write(f"There are **{total_found - total_fixed}** issues that could not be automatically fixed.\n")
                f.write("These require manual intervention:\n\n")
                
                f.write("1. Review the individual fixer logs for details\n")
                f.write("2. Check for issues that need architectural changes\n")
                f.write("3. Consider updating dependencies\n")
                f.write("4. Review deprecated functionality\n\n")
            
            if any(not r.success for r in results.values()):
                f.write("### Failed Fixers\n\n")
                f.write("Some fixers failed to complete. Check their log files for details:\n\n")
                
                for fixer_type, result in results.items():
                    if not result.success and result.log_file:
                        f.write(f"- {self.fixers[fixer_type]['name']}: `{result.log_file.name}`\n")
            
            f.write("\n## Next Steps\n\n")
            f.write("1. Review this report and individual fixer logs\n")
            f.write("2. Run the test suite to verify fixes\n")
            f.write("3. Commit the changes if everything looks good\n")
            f.write("4. Address any remaining manual fixes\n")
        
        self.logger.info(f"Summary report generated: {report_file}")
        return report_file
    
    def run_verification(self) -> bool:
        """Run basic verification after fixes"""
        self.logger.info("Running verification...")
        
        verifications = [
            ("CMake configuration", ["cmake", "-B", "build"]),
            ("Build compilation", ["cmake", "--build", "build", "--target", "all", "-j4"]),
            ("Run tests", ["ctest", "--test-dir", "build", "--output-on-failure"])
        ]
        
        all_passed = True
        
        for name, cmd in verifications:
            self.logger.info(f"Verifying: {name}")
            try:
                result = subprocess.run(
                    cmd,
                    cwd=self.project_root,
                    capture_output=True,
                    text=True,
                    timeout=600  # 10 minutes
                )
                
                if result.returncode == 0:
                    self.logger.info(f"✓ {name} passed")
                else:
                    self.logger.error(f"✗ {name} failed")
                    all_passed = False
                    
            except subprocess.TimeoutExpired:
                self.logger.error(f"✗ {name} timed out")
                all_passed = False
            except Exception as e:
                self.logger.error(f"✗ {name} error: {e}")
                all_passed = False
        
        return all_passed


def main():
    """Main entry point"""
    parser = argparse.ArgumentParser(
        description="OpenCog Master Fixing Tool",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
This tool orchestrates all OpenCog fixing utilities to provide a comprehensive
solution for various issues in the codebase.

Examples:
  # Fix everything automatically
  python fix-all.py
  
  # Dry run to see what would be fixed
  python fix-all.py --dry-run
  
  # Interactive mode
  python fix-all.py --interactive
  
  # Fix only specific types
  python fix-all.py --type test
  python fix-all.py --type cmake
  
  # Run verification after fixes
  python fix-all.py --verify
        """
    )
    
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show what would be fixed without making changes"
    )
    
    parser.add_argument(
        "--interactive",
        action="store_true",
        help="Interactively approve each fix"
    )
    
    parser.add_argument(
        "--type",
        choices=["test", "cmake", "scheme", "all"],
        default="all",
        help="Type of fixes to apply (default: all)"
    )
    
    parser.add_argument(
        "--no-parallel",
        action="store_true",
        help="Run fixers sequentially instead of in parallel"
    )
    
    parser.add_argument(
        "--verify",
        action="store_true",
        help="Run verification after applying fixes"
    )
    
    parser.add_argument(
        "--project-root",
        type=Path,
        default=Path.cwd(),
        help="OpenCog project root directory"
    )
    
    args = parser.parse_args()
    
    # Create orchestrator
    orchestrator = FixerOrchestrator(args.project_root)
    
    # ASCII art banner
    print("""
    ╔═══════════════════════════════════════════════════════════╗
    ║                                                           ║
    ║        OpenCog Master Fixing Tool - v1.0                  ║
    ║        Making Everything Amazing! ✨                      ║
    ║                                                           ║
    ╚═══════════════════════════════════════════════════════════╝
    """)
    
    # Determine which fixers to run
    if args.type == "all":
        print("🔧 Running all fixers...\n")
        results = orchestrator.run_all_fixers(
            dry_run=args.dry_run,
            interactive=args.interactive,
            parallel=not args.no_parallel
        )
    else:
        print(f"🔧 Running {args.type} fixer...\n")
        fixer_type = FixerType(args.type)
        result = orchestrator.run_fixer(
            fixer_type,
            dry_run=args.dry_run,
            interactive=args.interactive
        )
        results = {fixer_type: result}
    
    # Generate summary report
    report_file = orchestrator.generate_summary_report(results)
    
    # Display summary
    print("\n" + "="*60)
    print("SUMMARY")
    print("="*60)
    
    total_found = sum(r.issues_found for r in results.values())
    total_fixed = sum(r.issues_fixed for r in results.values())
    
    print(f"\n📊 Total issues found: {total_found}")
    print(f"✅ Total issues fixed: {total_fixed}")
    
    if total_found > 0:
        print(f"📈 Success rate: {total_fixed/total_found*100:.1f}%")
    
    print(f"\n📄 Detailed report: {report_file}")
    
    # Run verification if requested
    if args.verify and not args.dry_run:
        print("\n" + "="*60)
        print("VERIFICATION")
        print("="*60 + "\n")
        
        if orchestrator.run_verification():
            print("\n✅ All verifications passed!")
            return 0
        else:
            print("\n❌ Some verifications failed. Check the logs for details.")
            return 1
    
    # Return appropriate exit code
    if all(r.success for r in results.values()) and total_fixed == total_found:
        print("\n🎉 All issues successfully fixed!")
        return 0
    elif total_fixed > 0:
        print(f"\n⚠️  Partially successful: {total_fixed}/{total_found} issues fixed")
        return 2
    else:
        print("\n❌ No issues were fixed")
        return 1


if __name__ == "__main__":
    sys.exit(main()) 