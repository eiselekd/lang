#
#   finalizer.rb - 
#   	$Release Version: 0.3$
#   	$Revision: 1.4 $
#   	$Date: 1998/02/27 05:34:33 $
#   	by Keiju ISHITSUKA
#
# --
#
#   Usage:
#
#   add(obj, dependant, method = :finalize, *opt)
#   add_dependency(obj, dependant, method = :finalize, *opt)
#	add dependency R_method(obj, dependant)
#
#   delete(obj_or_id, dependant, method = :finalize)
#   delete_dependency(obj_or_id, dependant, method = :finalize)
#	delete dependency R_method(obj, dependant)
#   delete_all_dependency(obj_or_id, dependant)
#	delete dependency R_*(obj, dependant)
#   delete_by_dependant(dependant, method = :finalize)
#	delete dependency R_method(*, dependant)
#   delete_all_by_dependant(dependant)
#	delete dependency R_*(*, dependant)
#   delete_all
#	delete all dependency R_*(*, *)
#
#   finalize(obj_or_id, dependant, method = :finalize)
#   finalize_dependency(obj_or_id, dependant, method = :finalize)
#	finalize the dependant connected by dependency R_method(obj, dependtant).
#   finalize_all_dependency(obj_or_id, dependant)
#	finalize all dependants connected by dependency R_*(obj, dependtant).
#   finalize_by_dependant(dependant, method = :finalize)
#	finalize the dependant connected by dependency R_method(*, dependtant).
#   finalize_all_by_dependant(dependant)
#	finalize all dependants connected by dependency R_*(*, dependant).
#   finalize_all
#	finalize all dependency registered to the Finalizer.
#
#   safe{..}
#	stop invoking Finalizer on GC.
#

module Finalizer
  RCS_ID='-$Id: finalize.rb,v 1.4 1998/02/27 05:34:33 keiju Exp keiju $-'
  
  # @dependency: {id => [[dependant, method, *opt], ...], ...}
  
  # add dependency R_method(obj, dependant)
  def add_dependency(obj, dependant, method = :finalize, *opt)
    ObjectSpace.call_finalizer(obj)
    method = method.intern unless method.kind_of?(Integer)
    assoc = [dependant, method].concat(opt)
    if dep = @dependency[obj.id]
      dep.push assoc
    else
      @dependency[obj.id] = [assoc]
    end
  end
  alias add add_dependency
  
  # delete dependency R_method(obj, dependant)
  def delete_dependency(id, dependant, method = :finalize)
    id = id.id unless id.kind_of?(Integer)
    method = method.intern unless method.kind_of?(Integer)
    for assoc in @dependency[id]
      assoc.delete_if do
	|d, m, *o|
	d == dependant && m == method
      end
      @dependency.delete(id) if assoc.empty?
    end
  end
  alias delete delete_dependency
  
  # delete dependency R_*(obj, dependant)
  def delete_all_dependency(id, dependant)
    id = id.id unless id.kind_of?(Integer)
    method = method.intern unless method.kind_of?(Integer)
    for assoc in @dependency[id]
      assoc.delete_if do
	|d, m, *o|
	d == dependant
      end
      @dependency.delete(id) if assoc.empty?
    end
  end
  
  # delete dependency R_method(*, dependant)
  def delete_by_dependant(dependant, method = :finalize)
    method = method.intern unless method.kind_of?(Integer)
    for id in @dependency.keys
      delete(id, dependant, method)
    end
  end
  
  # delete dependency R_*(*, dependant)
  def delete_all_by_dependant(dependant)
    for id in @dependency.keys
      delete_all_dependency(id, dependant)
    end
  end
  
  # finalize the depandant connected by dependency R_method(obj, dependtant)
  def finalize_dependency(id, dependant, method = :finalize)
    id = id.id unless id.kind_of?(Integer)
    method = method.intern unless method.kind_of?(Integer)
    for assocs in @dependency[id]
      assocs.delete_if do
	|d, m, *o|
	d.send(m, id, *o) if ret = d == dependant && m == method
	ret
      end
      @dependency.delete(id) if assoc.empty?
    end
  end
  alias finalize finalize_dependency
  
  # finalize all dependants connected by dependency R_*(obj, dependtant)
  def finalize_all_dependency(id, dependant)
    id = id.id unless id.kind_of?(Integer)
    method = method.intern unless method.kind_of?(Integer)
    for assoc in @dependency[id]
      assoc.delete_if do
	|d, m, *o|
	d.send(m, id, *o) if ret = d == dependant
      end
      @dependency.delete(id) if assoc.empty?
    end
  end
  
  # finalize the dependant connected by dependency R_method(*, dependtant)
  def finalize_by_dependant(dependant, method = :finalize)
    method = method.intern unless method.kind_of?(Integer)
    for id in @dependency.keys
      finalize(id, dependant, method)
    end
  end
  
  # finalize all dependants connected by dependency R_*(*, dependtant)
  def finalize_all_by_dependant(dependant)
    for id in @dependency.keys
      finalize_all_dependency(id, dependant)
    end
  end
  
  # finalize all dependants registered to the Finalizer.
  def finalize_all
    for id, assocs in @dependency
      for dependant, method, *opt in assocs
	dependant.send(method, id, *opt)
      end
      assocs.clear
    end
  end
  
  # method to call finalize_* safely.
  def safe
    old_status = Thread.critical
    Thread.critical = true
    ObjectSpace.remove_finalizer(@proc)
    yield
    ObjectSpace.add_finalizer(@proc)
    Thread.critical = old_status
  end
  
  # registering function to ObjectSpace#add_finalizer
  def final_of(id)
    if assocs = @dependency.delete(id)
      for dependant, method, *opt in assocs
	dependant.send(method, id, *opt)
      end
    end
  end
  
  @dependency = Hash.new
  @proc = proc{|id| final_of(id)}
  ObjectSpace.add_finalizer(@proc)

  module_function :add
  module_function :add_dependency
  
  module_function :delete
  module_function :delete_dependency
  module_function :delete_all_dependency
  module_function :delete_by_dependant
  module_function :delete_all_by_dependant
  
  module_function :finalize
  module_function :finalize_dependency
  module_function :finalize_all_dependency
  module_function :finalize_by_dependant
  module_function :finalize_all_by_dependant
  module_function :finalize_all

  module_function :safe
  
  module_function :final_of
  private_class_method :final_of
  
end
