//==============================================================================================================================================================
// Implementation.

#include "Terse.h"
#pragma warning(disable:4100 4244 4706)
#if __clang__
#pragma clang diagnostic ignored "-Wlogical-op-parentheses"
#endif
using namespace Verse;

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Runtime errors.

static auto R00(const locus& Locus                       ) {return error{Locus,"R00"_VS,10,"ForForkStep"_VS};}
static auto R20(const locus& Locus,const char* What      ) {return error{Locus,"R20"_VS,10,ToString("Runtime fails where disallowed: "_VS,What)};}
static auto R03(const locus& Locus                       ) {return error{Locus,"R03"_VS,11,"Runtime unification stalled"_VS};}
static auto R04(const locus& Locus                       ) {return error{Locus,"R04"_VS,11,"Runtime unification of incomparable types stalled"_VS};}
static auto S00(const locus& Locus,const string& Internal) {return error{Locus,"S00"_VS,40,"Success"_VS,Internal};}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Contextual error handling.

// Always called in the context we're asking about.
static void ContextErrorHelper(const locus& OuterLocus,error& BestError,bool Dump,nat Depth,bool Everything) {
	// Traverse suspensions in program sequential order.
	for(auto Sus=Cast<suspension>(Thread->ContextResumeStep); Sus; Sus=Cast<suspension>(Sus->NextResumeStep)) {
		auto SusContext=Sus.Coerce().Cast<context>();
		if(Everything || !(Sus->SuspendedFx<=Thread->AllowFx)) {
			bool Recurse = true;
			auto Error   = Thread->OnDescribeSuspension(Sus.Coerce(),Recurse);
			Error.Locus  = Error.Locus.Else(OuterLocus);
			if(SusContext && SusContext->HoldFx<=Thread->AllowFx && Recurse && Error.ErrorCode!="P00")
				// Context effects are allowed but child effects aren't, so defer error to children.
				Error=S00(Error.Locus,Error.Internal);
			if(Dump)
				Print(For(Depth,[&](nat){return "    "_VS;}).Concatenate(),Error,
					!(Sus->SuspendedFx<=Thread->AllowFx)?
						ToString("; fx=",Sus->SuspendedFx," allowed=",Thread->AllowFx): False);
			if(Error.Priority<BestError.Priority) // Highest priority then outermost then sequentially earliest.
				BestError=Error;
			if(SusContext && (Recurse || Everything))
				SusContext->Run([&]{
					// Only update BestError if Recurse.
					return ContextErrorHelper(Error.Locus,Recurse? BestError: Error,Dump,Depth+1,Everything);
				});
		}
	}
}
error Verse::ContextError(const locus& Locus) {
	error Error=S00(Locus,False);
	return ContextErrorHelper(locus{},Error,false,0,false), Error;
}
void Verse::ContextReport(bool Everything) {
	Print("--- Context Report ---");
	error Error=S00(locus{},False);
	ContextErrorHelper(locus{},Error,true,0,Everything);
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Effects.

bool Verse::AllFxReady(fx Fx) {
	auto ThreadPendingFx = SequenceFx(Thread->ParentPendingFx,Thread->LocalPendingFx);
	fx   ReadyFx         = ReadyAfterFx(ThreadPendingFx)&(Thread->AllowFx+only_rejects);
	return Fx<=ReadyFx;
}
static bool AnyFxReady(fx Fx) {
	static const fx no_ready_triggers = abstracts&allocates&accepts;
	auto            ThreadPendingFx   = SequenceFx(Thread->ParentPendingFx,Thread->LocalPendingFx);
	fx              ReadyFx           = ReadyAfterFx(ThreadPendingFx)&(Thread->AllowFx+only_rejects);
	return !((Fx&ReadyFx)<=no_ready_triggers);
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Stepping.

void Verse::RunSteps(const function<current_step(const step&)>& F) {
	bool Done=false;
	current_step Current=F([&]()->current_step {Done=true; return BadStep;});
	while(!Done)
		Current=Current();
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Suspensions.

suspension_managed::suspension_managed(fx SuspendedFx0,fx WhenFx0): 
	managed_function<current_step()>(0), IsReady(false), SuspendedFx(SuspendedFx0), WhenFx(WhenFx0), NextResumeStep(BadStep) {}
current_step suspension_managed::OnRunSuspensionStep(const step& Step0) {
	VERSE_UNEXPECTED;
}
error suspension_managed::OnDescribe() const {
	return VERSE_HERE("NoSuspension")();
}
void suspension_managed::Suspend() {
	VERSE_ENSURE(Context()->Visibility()==Thread->Visibility());
	VERSE_ENSURE(!IsReady);
	//Print("SUSPEND ",OnDescribe());
	step SusStep = box<managed_function<current_step()>>(*this);
	if(Thread->HasSuspensions())
		Thread->ContextLastSuspension->NextResumeStep=SusStep, NextResumeStep=BadStep;
	else
		Thread->ContextResumeStep=SusStep;
	Thread->LocalPendingFx        = SequenceFx(Thread->LocalPendingFx,SuspendedFx);
	Thread->WhenFx               += WhenFx;
	Thread->ContextLastSuspension = *this;
}
current_step suspension_managed::operator()() const {
	VERSE_ASSERT(Context()->Visibility()==Thread->Visibility());
	auto* Self           = const_cast<suspension_managed*>(this);
	auto  Step1          = Exchange(Self->NextResumeStep,BadStep);
	if(IsReady || AnyFxReady(WhenFx))
		return Self->IsReady=false, Self->OnRunSuspensionStep(Step1);
	else
		return Self->Suspend(), Step1;
}
current_step Verse::RunSuspensionsStep(const step& Step0) {
	Thread->IsReady        = false;
	Thread->LocalPendingFx = Thread->StuckFx; // We handle ParentPendingFx elsewhere as it can change.
	Thread->WhenFx         = no_effects;
	if(Thread->HasSuspensions()) {
		// Tear off all of the suspensions we need to run in sequence then clear outstanding suspension.
		Thread->ContextLastSuspension->NextResumeStep = Step0;
		Thread->ContextLastSuspension                 = NoSuspension;
		return Exchange(Thread->ContextResumeStep,BadStep);
	}
	else return Step0;
};
current_step Verse::ResumeStep(const step& Step0) {
	if(!Thread->IsReady)
		return Step0;
	if(Thread->Depth)
		if(auto Parent=Thread->Context(); Parent->IsReady) {
			auto Child = Thread;
			Thread     = Parent;
			return ResumeStep([=]()->current_step {
				if(auto Step1=Child->EnterContext("ResumeStep")) // Doesn't set ParentStep when returning to child.
					return Step1.Coerce();
				return RunSuspensionsStep(Step0);
			});
		}
	return RunSuspensionsStep(Step0);
}
void Verse::Resume() {
	RunSteps(ResumeStep);
}
void Verse::ReadyCommonContext(context C1) {
	// Ready up to common context between specified context and thread context.
	// Don't try to simplify this to early out; the full logic is necessary in the general case.
	auto C0=Thread;
	while(C0!=C1) {
		auto D0=C0->Depth, D1=C1->Depth;
		if(D0>=D1)
			C0->IsReady=true, C0=C0.Context();
		if(D1>=D0)
			C1->IsReady=true, C1=C1.Context();
	}
	C0->IsReady=true;
}
void suspension_managed::ReadySuspensionBatch() {
	if(!IsReady)
		IsReady=true, ReadyCommonContext(Context());
}
step suspension_managed::ReadySuspensionStep(const step& Step0) {
	VERSE_ENSURE(!IsReady);
	IsReady=true;
	ReadyCommonContext(Context());
	return ResumeStep(Step0);
}
void when_future_suspension::OnCloned(cloner& Cloner) {
	if(!(Cloner.TargetIterate<=SuspensionFuture.Context())) {
		auto Work = kernel::non_value_accessor(SuspensionFuture);
		if(Work.NonValueType) { //!! Somehow we're finding live spent suspensions. 
			FutureNextSuspension=Work.Waiter();
			kernel::AddSuspension(Work,*this);
		}
	}
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// User suspensions.

struct suspension_step_managed: suspension_managed {
	function<current_step(const step&)> Step;
	suspension_step_managed(const function<current_step(const step&)>& Step0): Step(Step0) {}
	current_step OnRunSuspensionStep(const step& Step0) override {
		return Step(Step0);
	}
};
suspension Verse::SuspendStep(const function<current_step(const step&)>& Step) {
	auto Sus=box<suspension_step_managed>(Step);
	Sus->Suspend();
	return Sus;
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Contexts.

context_managed::context_managed(bool IsCommitted0,bool IsCommittable0,fx AllowFx0,fx HoldFx0,nat Depth0):
	suspension_managed    (only_succeeds), 
	IsCommitted           (IsCommitted0),
	IsCommittable         (IsCommittable0),
	AllowFx               (AllowFx0),
	ParentPendingFx       (only_succeeds),
	LocalPendingFx        (only_succeeds),
	StuckFx               (only_succeeds),
	HoldFx                (HoldFx0),
	Depth                 (Depth0),
	ContextLastSuspension (NoSuspension),
	ContextResumeStep     (BadStep) {}
pin<iterate> context_managed::Visibility() const {
	// This is a dynamic property of the context_managed.
	if(!IsCommitted)
		return box(*this).Coerce<iterate>();
	return Context()->Visibility();
}
error context_managed::OnDescribeSuspension(const suspension& Sus,bool& Recurse) const {
	return Sus->OnDescribe();
}
void Verse::StuckFx(fx Fx) {
	Thread->StuckFx        = SequenceFx(Thread->StuckFx,Fx);
	Thread->LocalPendingFx = SequenceFx(Thread->LocalPendingFx,Fx);
}
error context_managed::OnDescribe() const {
	return VERSE_HERE("context_managed")();
}
option<step> context_managed::EnterContext(const char* What) {
	VERSE_ENSURE(Context()==Thread);
	if(Thread->IsReady)
		IsReady=true;
	ParentPendingFx = only_succeeds +
		              ((IsCommitted? iterates: contradicts) & SequenceFx(Thread->ParentPendingFx,Thread->LocalPendingFx));
	Thread          = *this;
	return False;
}
static void EnterRecursive(const context& Target) {
	if(Target!=Thread) {
		VERSE_ENSURE(Target->Depth>0);
		EnterRecursive(Target.Context());
		VERSE_ENSURE(!Target->EnterContext("EnterRecursive"));
	}
}
current_step Verse::EnterRecursiveStep(const context& Target,const step& Step0) {
	EnterRecursive(Target);   // Could easily support Stops if needed.
	return ResumeStep(Step0); // Resumes immediately; if we took a Step0 that itself resumed, it wouldn't be current_step.
}
current_step context_managed::RunStep(const step& Step0,const function<current_step(const step&)>& F) {
	VERSE_ENSURE(!Thread->IsReady);
	VERSE_ENSURE(!EnterContext("RunStep"));
	return F([=,Child=box(*this)]()->current_step {
		VERSE_ENSURE(Thread==Child);
		VERSE_ENSURE(!Thread->IsReady);
		VERSE_ENSURE(!Thread.Context()->IsReady);
		Thread = Thread.Context();
		if(Child->HasSuspensions())
			Child->SuspendedFx=Child->LocalPendingFx, Child->Suspend();
		else VERSE_ENSURE(Child->LocalPendingFx==only_succeeds);
		return Step0;
	});
}
bool context_managed::HasSuspensions() const {
	return ContextLastSuspension!=NoSuspension;
}
current_step context_managed::OnRunSuspensionStep(const step& Step0) {
	return RunStep(Step0,RunSuspensionsStep);
}
step context_managed::TerminateStep(const step& Step0) {
	ContextLastSuspension = NoSuspension;
	return ReadySuspensionStep(Step0);
}
bool Verse::operator<=(const pin<context>& A,const pin<context>& B) {
	// Is A a parent of B?
	if(A->Depth<=B->Depth) {
		auto BP=B;
		auto Hops=B->Depth-A->Depth;
		while(Hops--)
			BP=BP->Context();
		return BP==A;
	}
	return false;
}
bool Verse::operator<(const pin<context>& A,const pin<context>& B) {
	// Is A a parent of B?
	if(A->Depth<B->Depth) {
		auto BP=B;
		auto Hops=B->Depth-A->Depth;
		while(Hops--)
			BP=BP->Context();
		return BP==A;
	}
	return false;
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Iteration contexts.

// Context control flow.
current_step iterate_managed::OnRunSuspensionStep(const step& Step0) {
	ParentStep=Step0;
	if(auto Step1=EnterContext("OnRunSuspensionStep"))
		return Step1.Coerce();
	return RunSuspensionsStep(LeaveIterateStep);
}
bool iterate_managed::Stopped() {
	return false;
}
current_step iterate_managed::ContextSucceeds(const step& Step0) const {
	VERSE_UNEXPECTED;
}
current_step iterate_managed::ContextFails(bool KeepWrites,const step& Step0) const {
	VERSE_UNEXPECTED;
}
iterate Verse::Internal::LeaveIterate() {
	auto Iterate = Thread.Coerce<iterate>();
	Thread       = Iterate->Context();
	if(Iterate->IterateFork && !Iterate->IsCommittable) {
		// Enqueue next implication fork ahead of ParentStep.
		//Print("LeaveIterate Enqueued IterateFork");
		auto[ForkContext,ForkStep] = Iterate->IterateFork.Coerce();
		ForkContext->ParentStep    = Iterate->ParentStep;
		Iterate->ParentStep        = ForkStep;
		Iterate->IterateFork       = False;
	}
	return Iterate;
}
tuple<bool,fx> iterate_managed::RefineIterateSuspendedFx(fx NewChildFx,bool UpdateHoldFx) {
	bool IsReleaseable = NewChildFx<=(succeeds&AllowFx)+contradicts || NewChildFx<=(fails&AllowFx)+contradicts;
	if(UpdateHoldFx && IsReleaseable)
		HoldFx &= succeeds&converges&computes&no_unifies; // Mutate so information is available elsewhere; keeps rejects.
	auto NewSuspendedFx = 
		(HoldFx     & (NewChildFx<=AllowFx && (IsReleased || UpdateHoldFx&&IsReleaseable || !IsCommittable)? accepts: effects)) +
		(NewChildFx & (IsCommitted? effects: IsCommittable? contradicts&no_unifies: contradicts&converges&no_transacts&no_imperatives&no_unifies));
	return {IsReleaseable,NewSuspendedFx};
}
void iterate_managed::OnRefineIterateFx() {}
current_step iterate_managed::OnLeftStep() {
	VERSE_ENSURE(!IsReady);
	if(OnRefineIterateFx(); Thread->IsReady) // If verifier effects recalculation readied anything.
		return ResumeStep([=,Self=box(*this)]()->current_step {
			return Self->OnRunSuspensionStep(Self->ParentStep);
		});
	auto[IsReleaseable,NewSuspendedFx] = RefineIterateSuspendedFx(LocalPendingFx,true);

	// Conditionally commit.
	if(LocalPendingFx<=succeeds && IsReleaseable && IsCommittable && !IsCommitted) {
		OnCommit();
		return ResumeStep([=,Self=box(*this)]()->current_step {
			return Self->OnRunSuspensionStep(Self->ParentStep);
		});
	}

	// Suspend, abandon, or keep.
	if(!(LocalPendingFx<=AllowFx) || IsCommittable&&HasSuspensions())
		SuspendedFx=NewSuspendedFx, Suspend();
	else if(!IsCommitted)
		OnAbandon();

	// Release notifications. !!rework to ensure confluence when <contradicts>.
	if(IsReleaseable && !IsReleased)
		return IsReleased=true, LocalPendingFx<=succeeds? ContextSucceeds(ParentStep): ContextFails(false,ParentStep);

	return Exchange(ParentStep,BadStep);
}
void iterate_managed::OnCommit() {
	IsCommitted=true;
}
void iterate_managed::OnAbandon() {}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Delimited control flow.

static void LeaveToIterate() {
	while(!Cast<iterate>(Thread))
		Thread=Thread->Context(), VERSE_ENSURE(!Thread->IsReady);
}
current_step Verse::FailStep(const locus& Locus,const char* What,const step& Step0) {
	if(fails<=contradicts+Thread->AllowFx) {
		LeaveToIterate();
		auto Child = Internal::LeaveIterate();
		return Child->ContextFails(false,Child->ParentStep);
	}
	else return Stuck(only_cardinalities&abstracts,R20(Locus,What)), Step0; // Don't ErrStep, in case there's another way out.
}
current_step Verse::ThrowStep(const locus& Locus,const future<>& a,const step& Step0) {
	// We suspend holding throws without succeeds, indicating that context will diverge.
	return WhenFxStep(VERSE_HERE("ThrowStep"),only_imperatives&throws,throws,Step0,[=](const step& Step1)->current_step {
		LeaveToIterate();
		auto Child = Internal::LeaveIterate();
		return Child->ContextThrows(Locus,a,Child->ParentStep);
	});
}
current_step Verse::ErrStep(const error& Error,const step& Step0) {
	LeaveToIterate();
	auto Child=Internal::LeaveIterate();
	return Child->ContextErrs(Error,Child->ParentStep);
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Unification.

static current_step UnifyHeadStep(const char* What,const locus& Locus,const pin<any>& A,const pin<any>& B,const step& Step0) {
	if(kernel::IsSameBox(A,B))
		return Step0;
	if(auto ASO=Cast<array<>>(A),BSO=Cast<array<>>(B); ASO && BSO) { // Generalize to map.
		if(Length(ASO.Coerce())==Length(BSO.Coerce())) {
			const auto& AS=ASO.PresumeReference(),BS=BSO.PresumeReference();
			if(auto n=Length(AS); n==Length(BS))
				return ForStep(n,Step0,[=](nat i,const step& Step1)->current_step {
					return UnifyStep("UnifyHeadStep-element",Locus,AS(i),BS(i),Step1);
				});
		}
		return FailStep(Locus,"UnifyHeadStep",Step0);
	}
	if(auto c=CompareDynamic(A,B); c==nullptr)
		return Step0;
	else if(!c.IsIncomparable())
		return FailStep(Locus,"UnifyHeadStep",Step0);
	else
		return Stuck(only_cardinalities&abstracts,R04(Locus)),Step0;
}
current_step Verse::UnifyStep(const char* What,const locus& Locus,const future<>& A,const future<>& B,const step& Step0) {
#if 1
	// Unification maximizing asynchronous progress requires:
	// - Never resolving flexible futures to inflexible non-heads.
	//   Here we do this.
	// - Localizing all inflexible unresolved futures.
	//   Here we do a subset, creating flexible local arrays when encountering inflexible array.
	if(auto AO=Cast<any>(A),BO=Cast<any>(B); AO&&BO)
		return UnifyHeadStep(What,Locus,AO.Coerce(),BO.Coerce(),Step0);
	else if(auto AF=IsFlexible(A),BF=IsFlexible(B); AF&&BF)
		return A.ResolveStep(B,Step0);
	else if(AF&&BO) {
		if(!BF)
			if(auto BSO=BO->Cast<array<>>()) {
				auto A1=For(Length(BSO.Coerce()),[&](nat){return future<>();});
				return A.ResolveStep(A1,[=]()->current_step {
					return UnifyHeadStep(What,Locus,A1,BO.Coerce(),Step0);
				});
			}
		return A.ResolveStep(BO.Coerce(),Step0);
	}
	else if(AO&&BF) {
		if(!AF)
			if(auto ASO=Cast<array<>>(AO.Coerce())) {
				auto B1=For(Length(ASO.Coerce()),[&](nat){return future<>();});
				return B.ResolveStep(B1,[=]()->current_step {
					return UnifyHeadStep(What,Locus,AO.Coerce(),B1,Step0);
				});
			}
		return B.ResolveStep(AO.Coerce(),Step0);
	}
	else if(AF) // Wait for B then re-unify.
		return WhenResolveStep([=]{return R03(Locus);},only_cardinalities&abstracts,B,Step0,[=](const any& B1,const step& Step1)->current_step {
			return UnifyStep(What,Locus,A,B1,Step1);
		});
	else if(BF) // Wait for A then re-unify.
		return WhenResolveStep([=]{return R03(Locus);},only_cardinalities&abstracts,A,Step0,[=](const any& A1,const step& Step1)->current_step {
			return UnifyStep(What,Locus,A1,B,Step1);
		});
	else // Wait for A and B then compare heads.
		return WhenResolveStep([=]{return R03(Locus);},only_cardinalities&abstracts,A,Step0,[=](const any& A1,const step& Step1)->current_step {
			return WhenResolveStep([=]{return R03(Locus);},only_cardinalities&abstracts,B,Step1,[=](const any& B1,const step& Step2)->current_step {
				return UnifyHeadStep(What,Locus,A1,B1,Step2);
			});
		});
#else
	// Old.
	if(auto AO=Cast<any>(A))
		if(auto BO=Cast<any>(B))
			return UnifyHeadStep(What,Locus,AO.Coerce(),BO.Coerce(),Step0);
	if(IsFlexible(A))
		return A.ResolveStep(B,Step0);
	if(IsFlexible(B))
		return B.ResolveStep(A,Step0);
	return WhenResolveStep([=]{return R03(Locus);},only_cardinalities&abstracts,A,Step0,[=](const any& A1,const step& Step1) {
		return WhenResolveStep([=]{return R03(Locus);},only_cardinalities&abstracts,B,Step1,[=](const any& B1,const step& Step2) {
			return UnifyHeadStep(What,Locus,A1,B1,Step2);
		});
	});
#endif
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Forking.

current_step Verse::ForForkStep(const locus& Locus,option<fx> ForHoldFx,nat i,nat n,const step& Step0,const function<current_step(nat,const step&)>& Steps) {
	if(i==n-1)
		return Steps(i,Step0);
	return WhenFxStep([=]{return R00(Locus);},only_cardinalities&(ForHoldFx? iterates: contradicts),ForHoldFx.Else(effects),Step0,[=](const step& Step1)->current_step {
		auto SavedContext = Thread;
		auto Iterate      = Thread->Visibility();
		while(!ForHoldFx && Iterate->IsCommittable)
			Iterate=Iterate.Context()->Visibility();

		// Fork captures current peer IterateFork before the assignment below replaces it with the new one.
		Iterate->IterateFork = Truth(kernel::CloneContext(Iterate,[=]()->current_step {
			VERSE_ENSURE(Thread==Iterate.Context());
			return EnterRecursiveStep(SavedContext,[=]()->current_step {
				return ForForkStep(Locus,ForHoldFx,i+1,n,Step1,Steps);
			});
		}));
		return Steps(i,Step1);
	});
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Async helpers.

void Verse::ErrorBatch(const error& Error) {
	struct error_suspension: suspension_managed {
		error Error;
		error_suspension(const error& Error0): suspension_managed(no_effects,no_effects), Error(Error0) {}
		current_step OnRunSuspensionStep(const step&) override {
			return ErrStep(Error);
		}
		error OnDescribe() const override {return Error;}
	};
	auto Sus=box<error_suspension>(Error);
	Sus->Suspend();
	Sus->ReadySuspensionBatch();
}
void Verse::Stuck(fx Fx,const error& Error) {
	WhenResolve([=]{return Error;},Fx,future<>(),[=](const any&)->current_step {VERSE_UNEXPECTED;});
}

void Verse::Internal::IterateFailsUnexpected   (               ) {VERSE_UNEXPECTED;}
void Verse::Internal::IterateThrowsUnexpected  (const future<>&) {VERSE_UNEXPECTED;}

current_step Verse::Internal::WhenCastThrowsUnexpected (const future<>&) {VERSE_UNEXPECTED;}
current_step Verse::Internal::WhenCastErrsUnexpected(const iterate&,const error&) {VERSE_UNEXPECTED;}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Futures.

VERSE_NO_INLINE void future<>::Resolve(const future<>& Target,bool ResolveLocal) const {
	if(ResolveBatch(Target,ResolveLocal))
		Resume();
}
current_step future<>::ResolveStep(const future<>& Target,const step& Step0,bool ResolveLocal) const {
	if(ResolveBatch(Target,ResolveLocal))
		return ResumeStep(Step0);
	else
		return Step0; // Nothing waiting.
}
