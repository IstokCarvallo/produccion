$PBExportHeader$w_mant_deta_spro_tarifafrutacomercial.srw
forward
global type w_mant_deta_spro_tarifafrutacomercial from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_spro_tarifafrutacomercial from w_mant_detalle_csd
integer width = 2834
integer height = 1204
event type boolean ue_duplicado ( string as_columna,  string as_valor )
end type
global w_mant_deta_spro_tarifafrutacomercial w_mant_deta_spro_tarifafrutacomercial

type variables
uo_especie 			 	iuo_especie
uo_variedades 		 	iuo_variedad
uo_grupoespecie    	iuo_grupo
uo_subgrupoespecie	iuo_subgrupo
uo_categorias		 	iuo_categoria	

DataWindowChild idwc_especies, idwc_grupo, idwc_variedad, idwc_subgrupo, &
						idwc_categoria
						
String					is_colname
Integer				ii_row
end variables

forward prototypes
public subroutine fechatermino ()
public function boolean duplicado (string as_columna, string as_valor)
end prototypes

event type boolean ue_duplicado(string as_columna, string as_valor);String 	espe_codigo, grva_codigo, grva_codsub, vari_codigo, cate_codigo, &
			refe_gcalib, tafc_preuni
Long 		ll_fila

espe_codigo		=	String(dw_1.Object.espe_codigo[il_fila])
grva_codigo		=	String(dw_1.Object.grva_codigo[il_fila])
grva_codsub		=	String(dw_1.Object.grva_codsub[il_fila])
vari_codigo		=	String(dw_1.Object.vari_codigo[il_fila])
cate_codigo		=	String(dw_1.Object.cate_codigo[il_fila])
refe_gcalib		=	String(dw_1.Object.refe_gcalib[il_fila])
tafc_preuni		=	String(dw_1.Object.tafc_preuni[il_fila])

CHOOSE CASE as_columna
	CASE "espe_codigo"
		espe_codigo	=	as_valor
		
	CASE "grva_codigo"
		grva_codigo	=	as_valor
		
	CASE "grva_codsub"
		grva_codsub	=	as_valor
		
	CASE "vari_codigo"
		vari_codigo	=	as_valor
		
	CASE "cate_codigo"
		cate_codigo	=	as_valor
		
	CASE "refe_gcalib"
		refe_gcalib	=	as_valor
		
	CASE "tafc_preuni"
		tafc_preuni	=	as_valor
		
END CHOOSE

ll_fila 	= 	dw_1.Find("espe_codigo = '" + espe_codigo + "'" +&
							 "grva_codigo = '" + grva_codigo + "'" +&
							 "grva_codsub = '" + grva_codsub + "'" +&
							 "vari_codigo = '" + vari_codigo + "'" +&
							 "cate_codigo = '" + cate_codigo + "'" +&
							 "refe_gcalib = '" + refe_gcalib + "'" +&
							 "tafc_preuni = '" + tafc_preuni + "'" , 1, dw_1.RowCount())
			 
IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
return true
end event

public subroutine fechatermino ();Long		ll_fila
String	ls_especie, ls_grupo, ls_subgrupo, ls_variedad, ls_categoria, ls_calibre, ls_Fecha
Date ldt_Fecha

ls_Fecha	= Mid(String(dw_1.Object.tafc_fecham[il_fila]),1,10)
ldt_Fecha = Date(ls_fecha)

ls_especie		= String(dw_1.Object.espe_codigo[il_fila])
ls_Grupo			= String(dw_1.Object.grva_codigo[il_fila])
ls_SubGrupo		= String(dw_1.Object.grva_codsub[il_fila])
ls_variedad		= String(dw_1.Object.vari_codigo[il_fila])
ls_categoria	= String(dw_1.Object.cate_codigo[il_fila])
ls_calibre		= dw_1.Object.refe_gcalib[il_fila]
		
ll_fila	= dw_1.Find("espe_codigo =" + ls_especie + " And grva_codigo ="  + ls_Grupo + &
							"And grva_codsub =" + ls_SubGrupo + " And vari_codigo =" + ls_variedad + &
							"And cate_codigo =" + ls_categoria + "And refe_gcalib ='" + ls_calibre + "'", &
							1, dw_1.RowCount())

IF ll_fila > 0 and ll_fila <> il_fila THEN
	dw_1.Object.tafc_fechat[ll_fila] = Date(ldt_fecha)
	
END IF

end subroutine

public function boolean duplicado (string as_columna, string as_valor);String 	espe_codigo, grva_codigo, grva_codsub, vari_codigo, cate_codigo, &
			refe_gcalib, tafc_preuni
Long 		ll_fila

espe_codigo		=	String(dw_1.Object.espe_codigo[il_fila])
grva_codigo		=	String(dw_1.Object.grva_codigo[il_fila])
grva_codsub		=	String(dw_1.Object.grva_codsub[il_fila])
vari_codigo		=	String(dw_1.Object.vari_codigo[il_fila])
cate_codigo		=	String(dw_1.Object.cate_codigo[il_fila])
refe_gcalib		=	String(dw_1.Object.refe_gcalib[il_fila])
tafc_preuni		=	String(dw_1.Object.tafc_preuni[il_fila])

CHOOSE CASE as_columna
	CASE "espe_codigo"
		espe_codigo	=	as_valor
		
	CASE "grva_codigo"
		grva_codigo	=	as_valor
		
	CASE "grva_codsub"
		grva_codsub	=	as_valor
		
	CASE "vari_codigo"
		vari_codigo	=	as_valor
		
	CASE "cate_codigo"
		cate_codigo	=	as_valor
		
	CASE "refe_gcalib"
		refe_gcalib	=	as_valor
		
	CASE "tafc_preuni"
		tafc_preuni	=	as_valor
		
END CHOOSE

ll_fila 	= 	dw_1.Find("espe_codigo = " + espe_codigo + " and " +&
							 "vari_codigo = " + vari_codigo + " and " +&
							 "cate_codigo = " + cate_codigo + " and " +&
							 "refe_gcalib = '" + refe_gcalib + "'" , 1, dw_1.RowCount())
//							 "grva_codigo = '" + grva_codigo + "' and " +&
//							 "grva_codsub = '" + grva_codsub + "' and " +&							 
//							 "tafc_preuni = '" + tafc_preuni + "'" , 1, dw_1.RowCount())
			 
IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
return true
end function

on w_mant_deta_spro_tarifafrutacomercial.create
call super::create
end on

on w_mant_deta_spro_tarifafrutacomercial.destroy
call super::destroy
end on

event open;x	= 100
y	= 450

This.Icon	=	Gstr_apl.Icono

istr_mant = Message.PowerObjectParm

iuo_especie				=	Create uo_especie
iuo_variedad			=	Create uo_variedades
iuo_grupo				=	Create uo_grupoespecie
iuo_subgrupo			=	Create uo_subgrupoespecie
iuo_categoria			=	Create uo_categorias

//Especie
dw_1.GetChild("espe_codigo",idwc_especies)
idwc_especies.SetTransObject(SQLCA)
idwc_especies.Retrieve()

//Grupo
dw_1.Getchild("grva_codigo",idwc_grupo)
idwc_grupo.SetTransObject(SQLCA)
IF idwc_grupo.Retrieve(0)=0 THEN
   idwc_grupo.insertrow(0)
END IF 

//SubGrupo
dw_1.Getchild("grva_codsub",idwc_subgrupo)
idwc_subgrupo.SetTransObject(SQLCA)
IF idwc_subgrupo.Retrieve(0,0)=0 THEN
   idwc_subgrupo.insertrow(0)
END IF	

//Variedad
dw_1.Getchild("vari_codigo",idwc_variedad)
idwc_variedad.SetTransObject(SQLCA)
IF idwc_variedad.Retrieve(0,0)=0 THEN
   idwc_variedad.insertrow(0)
END IF	

//Categoria
dw_1.GetChild("cate_codigo",idwc_categoria)
idwc_categoria.SetTransObject(SQLCA)
idwc_categoria.Retrieve()

PostEvent("ue_recuperadatos")

dw_1.SetTransObject(sqlca)
istr_mant.dw.ShareData(dw_1)
end event

event ue_nuevo();ib_ok = True

This.TriggerEvent("ue_guardar")

IF Message.DoubleParm = -1 THEN ib_ok = False

IF ib_ok = False THEN RETURN

wf_nuevo()

dw_1.Object.tafc_fecham[il_fila] = Date(istr_mant.argumento[1])
dw_1.Object.tafc_fechat[il_fila] = Date(istr_mant.argumento[2])

dw_1.SetColumn("espe_codigo")
dw_1.SetFocus()




end event

event ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]

IF Isnull(dw_1.Object.espe_codigo[il_fila]) OR dw_1.Object.espe_codigo[il_fila] = 0 THEN
   li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo de Especie"
	ls_colu[li_cont]	= "espe_codigo"
END IF

IF Isnull(dw_1.Object.vari_codigo[il_fila]) OR dw_1.Object.vari_codigo[il_fila] = 0 THEN
   li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo de Variedad"
	ls_colu[li_cont]	= "vari_codigo"
END IF

IF Isnull(dw_1.Object.cate_codigo[il_fila]) OR dw_1.Object.cate_codigo[il_fila] = 0 THEN
   li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCategoria"
	ls_colu[li_cont]	= "cate_codigo"
END IF

IF Isnull(dw_1.Object.refe_gcalib[il_fila]) OR dw_1.Object.refe_gcalib[il_fila] = "" THEN
   li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCalibre"
	ls_colu[li_cont]	= "refe_gcalib"
END IF

IF Isnull(dw_1.Object.tafc_preuni[il_fila]) OR dw_1.Object.tafc_preuni[il_fila] = 0 THEN
   li_cont ++
	ls_mensaje 			= ls_mensaje + "~nPrecio Unitario"
	ls_colu[li_cont]	= "tafc_preuni"
END IF



IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de : " + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus() 
	Message.DoubleParm = -1
END IF

FechaTermino()

 
end event

event ue_deshace();call super::ue_deshace;dw_1.Object.espe_codigo[il_fila]	=	integer(ias_campo[1])
dw_1.Object.grva_codigo[il_fila]	=	integer(ias_campo[2])
dw_1.Object.grva_codsub[il_fila]	=	integer(ias_campo[3])
dw_1.Object.vari_codigo[il_fila]	=	integer(ias_campo[4])
dw_1.Object.cate_codigo[il_fila]	=	integer(ias_campo[5])
dw_1.Object.refe_gcalib[il_fila]	=	ias_campo[6]
dw_1.Object.tafc_preuni[il_fila]	=	long(ias_campo[7])



end event

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[1]	=	String(dw_1.Object.espe_codigo[il_fila])
ias_campo[2]	=	String(dw_1.Object.grva_codigo[il_fila])
ias_campo[3]	=	String(dw_1.Object.grva_codsub[il_fila])
ias_campo[4]	=	String(dw_1.Object.vari_codigo[il_fila])
ias_campo[5]	=	String(dw_1.Object.cate_codigo[il_fila])
ias_campo[6]	=	String(dw_1.Object.refe_gcalib[il_fila])
ias_campo[7]	=	String(dw_1.Object.tafc_preuni[il_fila])

dw_1.GetChild("vari_codigo", idwc_variedad)
idwc_variedad.SetTransObject(sqlca)
idwc_variedad.Retrieve(dw_1.Object.espe_codigo[il_fila], 0)

IF istr_mant.agrega THEN
	dw_1.Object.tafc_fecham[il_fila] = Date(istr_mant.argumento[1])
	dw_1.Object.tafc_fechat[il_fila] = Date(istr_mant.argumento[2])
END IF


end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_spro_tarifafrutacomercial
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_spro_tarifafrutacomercial
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_spro_tarifafrutacomercial
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_spro_tarifafrutacomercial
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_spro_tarifafrutacomercial
integer x = 2510
integer y = 360
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_spro_tarifafrutacomercial
integer x = 2510
integer y = 180
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_spro_tarifafrutacomercial
integer x = 2510
integer y = 540
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_spro_tarifafrutacomercial
integer y = 104
integer width = 2208
integer height = 856
string dataobject = "dw_mant_deta_spro_tarifafrutacomercial"
end type

event dw_1::itemchanged;Integer	li_Null
String	ls_Columna, ls_Nula

ls_Columna = dwo.Name

SetNull(li_Null)
SetNull(ls_Nula)

CHOOSE CASE ls_Columna
	CASE "espe_codigo"	
		IF NOT iuo_Especie.Existe(Integer(Data),True,SqlCa) &
			OR Duplicado(ls_columna, data) THEN  
			This.SetItem(row,"espe_codigo", integer(ls_Nula))
			This.SetItem(row,'grva_codigo', Integer(ls_Nula))
			This.SetItem(row,'grva_codsub', Integer(ls_Nula))
			This.SetItem(row,'vari_codigo', Integer(ls_Nula))			
			This.SetFocus()
			RETURN 1
	   ELSE
			this.SetItem(row,'grva_codigo', Integer(ls_Nula))  
			dw_1.GetChild("grva_codigo",idwc_grupo)
			idwc_grupo.SetTransObject(Sqlca)
			idwc_grupo.Retrieve(integer(data))
			/**/
	 	  	This.SetItem(row,'grva_codsub', Integer(ls_Nula))
			dw_1.GetChild("grva_codsub",idwc_subgrupo)
			idwc_subgrupo.SetTransObject(Sqlca)
			idwc_subgrupo.Retrieve(integer(data),0)
			/**/
			this.SetItem(row,'vari_codigo', Integer(ls_Nula))
			dw_1.GetChild("vari_codigo",idwc_variedad)
			idwc_variedad.SetTransObject(Sqlca)
			idwc_variedad.Retrieve(integer(data),0)
		END IF	
	
	CASE "grva_codigo"
		IF NOT iuo_Grupo.Existe(dw_1.Object.espe_codigo[il_fila],Integer(data),True,SqlCa) &
			OR Duplicado(ls_columna, data) THEN
			This.SetItem(il_fila, "grva_codigo", Integer(ls_Nula))
			This.SetFocus()
			RETURN 1
		ELSE
			This.SetItem(il_fila, "grva_nombre_grupo", iuo_Grupo.nombregrupo)
			this.SetItem(il_fila,'grva_codsub', Integer(ls_Nula))
			this.SetItem(il_fila,'vari_codigo', Integer(ls_Nula))
			dw_1.GetChild("grva_codsub",idwc_subgrupo)
			idwc_subgrupo.SetTransObject(Sqlca)
			idwc_subgrupo.Retrieve(dw_1.Object.espe_codigo[il_fila],integer(data))
		
			idwc_variedad.SetFilter("grva_codigo=" + Data)
			idwc_variedad.Filter()
		END IF	
		
	CASE "grva_codsub"
		IF NOT iuo_SubGrupo.Existe(dw_1.Object.espe_codigo[il_fila],dw_1.Object.grva_codigo[il_fila],Integer(data),True,SqlCa) &
			OR Duplicado(ls_columna, data) THEN
			This.SetItem(il_fila, "grva_codsub", integer(ls_Nula))
			This.SetFocus()
			RETURN 1
		ELSE	
			This.SetItem(il_fila, "grva_nombre_subgrupo", iuo_SubGrupo.nombregrupo)
			this.SetItem(il_fila,'vari_codigo', Integer(ls_Nula))
			idwc_variedad.SetFilter("grva_codigo=" + String(dw_1.Object.grva_codigo[il_fila]) + &
		                        " And grva_codsub=" + Data)
			idwc_variedad.Filter()
  
		END IF
		
	CASE "vari_codigo"
		IF NOT iuo_variedad.existe(dw_1.Object.espe_codigo[il_fila],integer(data),True,SQLCA) &
			OR Duplicado(ls_columna, data) THEN
			This.SetItem(il_fila, "vari_codigo", integer(ls_Nula))
			This.SetFocus()
			RETURN 1
		ELSE
			IF NOT iuo_Grupo.Existe(dw_1.Object.espe_codigo[il_fila],iuo_variedad.grupo,True,SqlCa) THEN
				This.SetItem(il_fila, "grva_codigo", iuo_variedad.grupo)
				This.SetItem(il_fila, "grva_nombre_grupo", iuo_Grupo.nombregrupo)
			END IF	
			
			IF NOT iuo_SubGrupo.Existe(dw_1.Object.espe_codigo[il_fila],iuo_variedad.grupo,iuo_variedad.subgrupo,True,SqlCa) THEN
				This.SetItem(il_fila, "grva_codsub", iuo_variedad.subgrupo)
				This.SetItem(il_fila, "grva_nombre_subgrupo", iuo_SubGrupo.nombregrupo)
			END IF
			This.SetItem(il_fila, "variedades_vari_nombre", iuo_variedad.nombrevariedad)
		END IF
		
	CASE "cate_codigo"
		IF Not iuo_categoria.existe(integer(data),true,SQLCA) &
			OR Duplicado(ls_columna, data) THEN
			This.SetItem(il_fila, "cate_codigo", integer(ls_Nula))
			This.SetFocus()
			RETURN 1
		END IF	
	CASE "tafc_preuni"
		IF Duplicado(ls_columna, data) THEN
			This.SetItem(row,"espe_codigo", integer(ls_Nula))
			This.SetItem(row,'grva_codigo', Integer(ls_Nula))
			This.SetItem(row,'grva_codsub', Integer(ls_Nula))
			This.SetItem(row,'vari_codigo', Integer(ls_Nula))		
			This.SetItem(row,'cate_codigo', integer(ls_Nula))
			This.SetItem(row,'refe_gcalib', ls_Nula)
			This.SetItem(row,'tafc_preuni', integer(ls_Nula))
			This.SetFocus()
			Return 1
		END IF	
		
END CHOOSE
end event

event dw_1::itemfocuschanged;call super::itemfocuschanged;IF ii_row > 0 AND ii_row <= This.RowCount()THEN
	IF is_colname = "tafc_preuni" THEN
		This.Object.tafc_prebru[ii_row]	=	Round(This.Object.tafc_preuni[ii_row]	*	1.19, 2)
	ELSE
		This.Object.tafc_preuni[ii_row]	=	Round(This.Object.tafc_prebru[ii_row]	/	1.19, 2)
	END IF
END IF

ii_row = row
is_colname = dwo.Name
end event

